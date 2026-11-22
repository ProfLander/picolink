#lang picolink/backend

(require racket/string
         racket/list
         racket/set
         racket/path
         racket/file
         racket/syntax

         (except-in syntax/parse expr)

         picolink/alist
         picolink/artifacts
         picolink/scope
         picolink/binding
         picolink/config/parameters
         picolink/s-lua
         picolink/s-lua/lua
         picolink/program/scope
         picolink/module/compiled
         picolink/module/import
         picolink/module/export
         picolink/module/parameters

         (for-template racket/base
                       picolink/s-lua/ir
                       picolink/lua/run))

(provide (all-defined-out)
         (for-template (all-from-out picolink/lua/run)))

#:target       lua-target
#:search-paths lua-search-paths
#:modules      lua-modules
#:compile      lua-compile
#:run          lua-run
#:interact     lua-interact

(define current-debug
  (make-parameter #f))

(define (lua-search-paths)
  (list 'picolink/lua/modules))

(define (lua-modules)
  (list #'lua/runtime))

(define (subprocess-ident lctx)
  (datum->syntax lctx '#%subprocess))

(define (stdout-ident lctx)
  (datum->syntax lctx '#%stdout))

(define (stdin-ident lctx)
  (datum->syntax lctx '#%stdin))

(define (stderr-ident lctx)
  (datum->syntax lctx '#%stderr))

(define lua-target
  (make-parameter 's-lua))

(define (entry-point-module?)
  (equal? (syntax-e (current-module-ident)) 'entry-point))

(define (module-body-empty? stx)
  (syntax-parse stx
    #:literal-sets [s-lua-ir-literals]
    [(#%chunk (#%block)) #t]
    [_ #f]))

(define (module-requires-empty?)
  (scope-empty? (current-require-scope)))

(define (module-provides-empty?)
  (null? (module-provides)))

(define (module-empty? stx)
  (and (module-requires-empty?)
       (module-provides-empty?)
       (module-body-empty? stx)))

(define stx-runtime-pre
  (in-s-lua
   #'(#%chunk
      (#%block
       (#%local [runtime]
                [(#%call require "lua.runtime")])

       (#%call (#%member runtime lua-pre) _NAME)))))

(define stx-runtime-post
  (in-s-lua
   #'(#%chunk
      (#%block
       (#%call (#%member runtime lua-post) _NAME)))))

(define (splice-runtime-pre stx)
  (s-lua-append stx-runtime-pre stx))

(define (splice-runtime-post stx)
  (s-lua-append stx stx-runtime-post))


(define current-lua-transform-module-body-pre
  (make-parameter splice-runtime-pre))

(define current-lua-transform-module-body-post
  (make-parameter splice-runtime-post))

(define (lua-transform-module stx)

  (if (module-empty? stx)

      #f

      (let-values ([(hidden-imports stx) (rewrite-idents stx)])

        (let* ([is-entry-point (entry-point-module?)]
               [stx (splice-hidden-imports hidden-imports stx)]
               [stx (splice-requires stx)]
               [stx (splice-scope stx)]
               [stx (if is-entry-point
                        ((current-lua-transform-module-body-pre) stx)
                        stx)]
               [stx (if is-entry-point
                        ((current-lua-transform-module-body-post) stx)
                        stx)]
               [stx (splice-module-name stx)]
               [stx (splice-provides stx)])

          stx))))

(define (lua-link lctx prog)

  (define linker (current-lua-linker))

  (unless linker
    (error "no linker specified"))

  (linker lctx prog))

(define (dotted-path->member-access path)
  (define segs (map string->symbol (string-split path ".")))

  (for/foldr ([acc (last segs)])
             ([seg (in-list (drop-right segs 1))])
    (with-syntax ([acc acc]
                  [seg seg])
      #`(#%member seg acc))))

(define (module-ident->lua-ident path)
  (string->symbol
   (string-replace
    (rewrite-name path)
    "/" "_")))

(define (module-ident->lua-string path)
  (string-replace
   (symbol->string (syntax-e path)) "/" "."))

(define (make-hygienic-import-stx src-binds members)

  (define source-bindings
    (for/list ([pair (in-list src-binds)])
      (with-syntax ([src-mod (car pair)]
                    [binding (cdr pair)])
        #'(#%local [src-mod] [binding]))))

  (define member-declarations
    (for/list ([pair (in-list members)])
      (with-syntax ([member (car pair)])
        #'(#%local [member]))))

  (define member-assignments
    (for/list ([pair (in-list members)])

      (with-syntax ([member (car pair)]
                    [value  (cdr pair)])

        #'(#%assign [member] [value]))))

  

  (in-s-lua
   (with-syntax
     ([(bind ...)    source-bindings]
      [(declare ...) member-declarations]
      [(assign ...)  member-assignments])

     #'(#%chunk
        (#%block
         declare ...
         (#%do
          (#%block
           bind ...
           assign ...)))))))

(define (splice-hidden-imports hidden-imports stx)

  (define scope-idents
    (for/set ([pair (in-set hidden-imports)])
      (car pair)))

  (define scope-bindings
    (for/list ([scope-ident (in-set scope-idents)])

      (let* ([scope-ident (datum->syntax #f scope-ident)]
             [scope-sym (module-ident->lua-ident scope-ident)])

        (with-syntax ([scope-str (module-ident->lua-string scope-ident)])

          (cons scope-sym #'(#%member scopes scope-str))))))

  (define members
    (for/list ([pair (in-set hidden-imports)])
      (let* ([src-mod (car pair)]
             [member  (cdr pair)]
             [member*  (format-id #f "~a-~a" src-mod member)])

        (with-syntax ([scope-sym (module-ident->lua-ident
                                  (datum->syntax #f src-mod))]
                      [member member])

          (cons member* #'(#%member scope-sym member))))))

  (if (pair? members)

      (s-lua-append
       (make-hygienic-import-stx scope-bindings members)
       stx)

      stx))

(define (splice-requires stx)

  (define (not-empty? ident)
    (not (module-empty?
          (with-scope (current-program-scope)
            (lookup ident)))))

  (define module-bindings
    (for/list ([req-mod (in-list (reverse
                                  (filter not-empty?
                                          (scope-names
                                           (current-require-scope)))))])

      (let ([src-mod-sym (module-ident->lua-ident req-mod)])

        (with-syntax ([src-mod-str (module-ident->lua-string req-mod)])

          (cons src-mod-sym #'(#%call require src-mod-str))))))

  (define members
    (for*/list ([req (in-list (module-requires))])

      (let ([to (import-local-id req)])

        (with-syntax ([src-mod-path (import-src-mod-path req)]
                      [from (import-src-sym req)])

          (cons to #'(#%member src-mod-path from))))))

  (define scope-assignments
    (for/list ([pair (in-list members)])

      (with-syntax ([to (car pair)])
        #'(#%assign [(#%member scope to)] [to]))))

  (if (or (pair? module-bindings)
          (pair? members))
      
      (s-lua-append

       (make-hygienic-import-stx
        module-bindings
        members)

       (in-s-lua
        (with-syntax ([(assign-scope ...)  scope-assignments])
          #'(#%chunk
             (#%block
              assign-scope ...))))

       stx)

      stx))

(define (splice-module-name stx)

  (s-lua-append
   (in-s-lua
    (with-syntax ([module-name (module-ident->lua-string
                                (current-module-ident))])
      #'(#%chunk
         (#%block
          (#%local [_NAME] [module-name])))))
   stx))

(define (splice-scope stx)

  (define splice
    (syntax-parser
      #:literal-sets [s-lua-ir-literals]

      [(#%chunk (#%block body ...))
       (in-s-lua
        (with-syntax ([(body ...) (apply append
                                         (map splice (attribute body)))])
          (syntax/loc this-syntax
            (#%chunk
             (#%block
              body ...)))))]

      [(#%local (#%function name _ _...))
       (list this-syntax
             (in-s-lua
              (syntax/loc this-syntax
                (#%assign [(#%member scope name)]
                          [name]))))]

      [(#%local [ident ...] _)
       (list this-syntax
             (in-s-lua
              (syntax/loc this-syntax
                (#%assign [(#%member scope ident) ...]
                          [ident ...]))))]

      [(#%do (#%block _ ... (#%assign [ident ...] _)))
       (list this-syntax
             (in-s-lua
              (syntax/loc this-syntax
                (#%assign [(#%member scope ident) ...]
                          [ident ...]))))]

      [_ (list this-syntax)]))

  (s-lua-append
   (in-s-lua
    #'(#%chunk
       (#%block

        (#%if (#%not scopes)
              (#%then
               (#%block
                (#%assign [scopes] [(#%table)]))))

        (#%local [scope] [(#%table)])

        (#%assign [(#%member scopes '_NAME)] [scope]))))

   (splice stx)))

(define (module-provides)

  (for/list ([(ident export) (in-scope (current-provide-scope))]
             
             #:do [(define mod-bind
                     (with-scope (current-lexical-scope)
                       (lookup ident)))]

             #:when (not (macro? mod-bind)))

    (cons (export-local-id export)
          (export-out-id export))))

(define (splice-provides stx)

  (if (scope-empty? (current-provide-scope))

      stx

      (s-lua-append
       stx
       (in-s-lua
        (with-syntax ([((prov-from . prov-to) ...) (module-provides)])
          #'(#%chunk
             (#%block
              (#%return (#%table [prov-to prov-from] ...)))))))))

(define (rewrite-idents stx)
  (define (rewrite acc stx)
    (syntax-parse stx
      [(form ...)

       (let-values ([(acc forms)
                     (for/fold ([acc acc]
                                [forms null])
                               ([form (in-list (attribute form))])
                       (let-values ([(acc form) (rewrite acc form)])
                         (values acc (cons form forms))))])

         (values acc (datum->syntax this-syntax
                                    (reverse forms)
                                    this-syntax
                                    this-syntax)))]

      [ident:id

       (define bound-from (syntax-property this-syntax 'bound-from))

       (if bound-from
           (values (set-add acc (cons (syntax-e bound-from)
                                      (syntax-e this-syntax)))
                   (format-id this-syntax "~a-~a" bound-from this-syntax))
           (values acc this-syntax))]

      [_ (values acc this-syntax)]))

  (let-values ([(acc stx) (rewrite (set) stx)])
    (values acc stx)))

(define (program-entry-point prog)
  (with-scope prog
    (lookup 'entry-point)))

(define (module-requires)

  (define require-scope (current-require-scope))

  (reverse
   (for*/list ([(_ imports) (in-scope require-scope)]
               [(_ import)  (in-alist imports)]

               #:do [(define mod-path (import-src-mod-path import))

                     (define mod
                       (with-scope (current-program-scope)
                         (lookup mod-path)))

                     (define src-sym
                       (import-src-sym import))

                     (define import-bind
                       (with-scope (picolink-module-lexical-scope mod)
                         (lookup (datum->syntax #f src-sym))))]

               #:when (and import-bind
                           (not (macro? import-bind))))

     import)))

(define (lua-compile stx)
  (let ([stx ((current-lua-transform-module) stx)])
    (when stx
      
      (let* ([stx ((s-lua-compiler 'lua) stx)]

             [name (symbol->string
                    (syntax-e
                     (current-module-ident)))]

             [path (path-replace-extension (string->path name) ".lua")])

        (set-compile-artifact! path (artifact/string stx))))))

(define (lua-link/collection lctx prog)

  (define compile (s-lua-compiler 'lua))

  (let ([modules (for/hash ([mod (in-list prog)])

                   (let* ([name (symbol->string
                                 (syntax-e
                                  (picolink-module-ident mod)))]
                          [path (string->path name)])
                     (values (path-replace-extension path ".lua")
                             (compile (picolink-module-stx mod)))))]

        [build-directory (build-path (path-only (current-config-path))
                                     (current-build-directory)
                                     (current-lua-build-subdir))])

    (delete-directory/files build-directory #:must-exist? #f)
    (make-directory* build-directory)

    (for ([(path mod) (in-hash modules)])

      (when (current-debug)
        (displayln (format "~a:\n" path))
        (displayln mod)
        (displayln ""))

      (let ([path (build-path build-directory path)])

        (make-directory* (path-only path))
        (display-to-file mod path #:exists 'replace)
        (flush-output)))

    (symbol->string
     (syntax-e
      (picolink-module-ident
       (program-entry-point prog))))))

(define (lua-run lctx prog)

  (define exe-name (current-lua-executable))

  (define exe (find-executable-path exe-name) )

  (unless exe
    (error (format "unable to locate ~a executable" exe-name)))

  (with-syntax* ([#%subprocess (subprocess-ident lctx)]
                 [#%stdout (stdout-ident lctx)]
                 [#%stdin (stdin-ident lctx)]
                 [#%stderr (stderr-ident lctx)]
                 [exe-name exe-name]
                 [exe exe]
                 [prog prog]
                 [do-run
                  (with-syntax ([runner (current-lua-runner)]
                                [build-directory
                                 (build-path
                                  (path-only (current-config-path))
                                  (current-build-directory)
                                  (current-lua-build-subdir))])
                    #'(runner exe build-directory prog))])
    #'(begin
        (define-values (#%subprocess #%stdout #%stdin #%stderr)
          do-run)

        (lua-listen exe-name #%subprocess #%stdout #%stdin #%stderr))))

(define (lua-interact lctx stx)

  (define compiler (s-lua-compiler 'lua))

  (let ([interaction (parameterize ([current-separator "; "])
                       (compiler stx))])

    (with-syntax ([#%stdin (stdin-ident lctx)]
                  [interaction interaction])

      #'(begin
         (when (port-closed? #%stdin)
           (raise-user-error
            (format "lua port is closed\n  port: ~a" #%stdin)))

         (displayln interaction #%stdin)
         (flush-output #%stdin)))))

; Parameters

(define current-lua-executable
  (make-parameter "lua"))

(define current-lua-build-subdir
  (make-parameter "lua"))

(define current-lua-linker
  (make-parameter #f))

(define current-lua-runner
  (make-parameter #f))

(define current-lua-entry-point-name
  (make-parameter #f))

(define current-lua-transform-module
  (make-parameter lua-transform-module))
