#lang picolink/backend

(require racket/function
         racket/string
         racket/list
         racket/set
         racket/dict
         racket/path
         racket/file
         racket/syntax

         (except-in syntax/parse expr)

         picolink/ee-lib
         picolink/binding
         picolink/config/parameters
         picolink/s-lua
         picolink/s-lua/lua
         picolink/module/compiled
         picolink/module/import
         picolink/module/export

         (for-template racket/base
                       picolink/s-lua/ir
                       picolink/lua/run))

(provide (all-defined-out)
         (for-template (all-from-out picolink/lua/run)))

#:target       lua-target
#:search-paths lua-search-paths
#:modules      lua-modules
#:transform    lua-transform
#:link         lua-link
#:run          lua-run
#:interact     lua-interact

(define current-debug
  (make-parameter #t))

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

(define (lua-transform lctx prog)

  (let ([transform-mod (current-lua-transform-module)])

    (for/list ([mod (in-list prog)]

               #:do [(define transformed
                       (and (compiled-module-stx mod)
                            (transform-mod lctx prog mod)))]

               #:when (and transformed
                           (compiled-module-stx transformed)))

      transformed)))

(define (entry-point-module? mod)
  (equal? (syntax-e (compiled-module-ident mod))
                                 'entry-point))

(define (module-body-empty? mod)
  (syntax-parse (compiled-module-stx mod)
    [(#%chunk (#%block)) #t]
    [_ #f]))

(define (module-requires-empty? prog mod)
  (null? (module-requires prog mod)))

(define (module-provides-empty? mod)
  (null? (module-provides mod)))

(define (module-empty? prog mod)
  (and (module-requires-empty? prog mod)
       (module-provides-empty? mod)
       (module-body-empty? mod)))

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

(define (splice-runtime-pre mod)
  (compiled-module-update
   mod
   #:stx (s-lua-append
          stx-runtime-pre
          (compiled-module-stx mod))))

(define (splice-runtime-post mod)
  (compiled-module-update
   mod
   #:stx (s-lua-append (compiled-module-stx mod)
                       stx-runtime-post)))


(define current-lua-transform-module-body-pre
  (make-parameter splice-runtime-pre))

(define current-lua-transform-module-body-post
  (make-parameter splice-runtime-post))

(define (lua-transform-module _lctx prog mod)
  (if (module-empty? prog mod)

      (compiled-module-update mod #:stx #f)

      (let-values ([(hidden-imports mod) (rewrite-idents mod)])

        (let* ([is-entry-point (entry-point-module? mod)]
               [mod (if is-entry-point
                        ((current-lua-transform-module-body-pre) mod)
                        mod)]
               [mod (splice-hidden-imports hidden-imports mod)]
               [mod (splice-requires prog mod)]
               [mod (splice-lctx mod)]
               [mod (if is-entry-point
                        ((current-lua-transform-module-body-post) mod)
                        mod)]
               [mod (splice-module-name mod)]
               [mod (splice-provides mod)])

          mod))))

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

(define (splice-hidden-imports hidden-imports mod)

  (define scope-idents
    (for/set ([pair (in-set hidden-imports)])
      (car pair)))

  (define scope-bindings
    (for/list ([scope-ident (in-set scope-idents)])

      (let* ([scope-ident (datum->syntax #f scope-ident)]
             [scope-sym (module-ident->lua-ident scope-ident)])

        (with-syntax ([scope-str (module-ident->lua-string scope-ident)])

          (cons scope-sym #'(#%member lctx scope-str))))))

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

      (compiled-module-update
       mod
       #:stx
       (s-lua-append
        (make-hygienic-import-stx scope-bindings members)
        (compiled-module-stx mod)))

      mod))

(define (splice-requires prog mod)

  (define (not-empty? ident)
    (not (module-empty? prog (program-module prog (syntax-e ident)))))

  (define module-bindings
    (for/list ([req-mod (in-list (reverse
                                  (filter not-empty?
                                          (scope-idents
                                           (compiled-module-require-scope
                                            mod)))))])

      (let ([src-mod-sym (module-ident->lua-ident req-mod)])

        (with-syntax ([src-mod-str (module-ident->lua-string req-mod)])

          (cons src-mod-sym #'(#%call require src-mod-str))))))

  (define members
    (for*/list ([req (in-list (module-requires prog mod))])

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
      
      (compiled-module-update
       mod
       #:stx
       (s-lua-append

        (make-hygienic-import-stx
         module-bindings
         members)

        (in-s-lua
         (with-syntax ([(assign-scope ...)  scope-assignments])
           #'(#%chunk
              (#%block
               assign-scope ...))))

        (compiled-module-stx mod)))

      mod))

(define (splice-module-name mod)

  (let ([stx (compiled-module-stx mod)])

    (compiled-module-update
     mod
     #:stx (s-lua-append
            (in-s-lua
             (with-syntax ([module-name (module-ident->lua-string
                                         (compiled-module-ident mod))])
               #'(#%chunk
                  (#%block
                   (#%local [_NAME] [module-name])))))
            stx))))

(define (splice-lctx mod)

  (define stx (compiled-module-stx mod))

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

      [_ (list this-syntax)]))

  (compiled-module-update
   mod
   #:stx (s-lua-append
          (in-s-lua
           #'(#%chunk
              (#%block

               (#%if (#%not lctx)
                     (#%then
                      (#%block
                       (#%assign [lctx] [(#%table)]))))

               (#%local [scope] [(#%table)])

               (#%assign [(#%member lctx '_NAME)] [scope]))))

          (splice stx))))

(define (module-provides mod)

  (define provide-scope
    (compiled-module-provide-scope mod))

  (for/list ([(ident export) (in-scope provide-scope)]
             
             #:do [(define mod-bind
                     (module-binding mod (syntax-e ident)))]

             #:when (not (macro? mod-bind)))

    (cons (export-local-id export)
          (export-out-id export))))

(define (splice-provides mod)

  (define provs
    (module-provides mod))

  (if (pair? provs)

      (let ([stx (compiled-module-stx mod)])

        (compiled-module-update
         mod
         #:stx (s-lua-append
                stx
                (in-s-lua
                 (with-syntax ([((prov-from . prov-to) ...) provs])
                   #'(#%chunk
                      (#%block
                       (#%return (#%table [prov-to prov-from] ...)))))))))

      mod))

(define (lift-package-preloader path stx)
  (syntax-parse stx
    #:literal-sets [s-lua-ir-literals]

    [(#%chunk (#%block body ...))
     (with-syntax ([path (module-ident->lua-string path)])
       #'(#%assign
          [(#%member (#%member package preload)
                     path)]
          [(#%function
            (#%vararg)
            (#%block
             body ...))]))]))

(define (rewrite-idents mod)
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

  (define-values (acc stx)
    (rewrite (set) (compiled-module-stx mod)))

  (values acc (compiled-module-update mod #:stx stx)))

(define (program-module prog sym)
  (findf (λ (mod)
           (equal? sym
                   (syntax-e
                    (compiled-module-ident mod))))
         prog))

(define (program-entry-point prog)
  (program-module prog 'entry-point))

(define (module-requires prog mod)

  (define require-scope (compiled-module-require-scope mod))

  (reverse
   (for*/list ([(mod-path imports) (in-scope require-scope)]
               [(_ import)         (in-dict imports)]

               #:do [(define mod-path (import-src-mod-path import))

                     (define import-mod
                       (program-module prog (syntax-e mod-path)))

                     (define import-bind
                       (module-binding import-mod
                                       (import-src-sym import)))]

               #:when (not (macro? import-bind)))

     import)))

(define (module-binding mod sym)

  (define mod-scope
    (compiled-module-module-scope mod))

  (define tail
    (member sym
            (scope-idents mod-scope)
            (λ (a b)
              (equal? a (syntax-e b)))))

  (define ident
    (and tail (car tail )))

  (and ident
       (parameterize ([current-def-ctx mod-scope])
         (lookup ident))))

(define (lua-link/chunk _lctx prog)

  (let* ([entry-point (program-entry-point prog)]
         
         [dependencies
          (for/list ([mod (in-list prog)]
                     #:when (not (eq? mod entry-point)))

            (lift-package-preloader (compiled-module-ident mod)
                                    (compiled-module-stx mod)))]

         [stx
          (in-s-lua
           (with-syntax ([(dependency ...) dependencies]
                         [(entry-point-body ...)
                          (syntax-parse (compiled-module-stx
                                         entry-point)
                            #:literal-sets [s-lua-ir-literals]
                            [(#%chunk (#%block body ...))
                             (attribute body)])])
             #'(#%chunk
                (#%block
                 dependency ...
                 entry-point-body ...))))]

         [compile  (s-lua-compiler 'lua)]

         [compiled (compile stx)])

    (when (current-debug)
      (displayln "lua:")
      (displayln compiled)
      (displayln ""))

    compiled))

(define (lua-link/collection lctx prog)

  (define compile (s-lua-compiler 'lua))

  (let ([modules (for/hash ([mod (in-list prog)])

                   (let* ([name (symbol->string
                                 (syntax-e
                                  (compiled-module-ident mod)))]
                          [rename (current-lua-entry-point-name)]
                          [name (if (and rename
                                         (equal? name "entry-point"))
                                    rename
                                    name)]
                          [path (string->path name)])
                     (values (path-replace-extension path ".lua")
                             (compile (compiled-module-stx mod)))))]

        [build-directory (build-path (current-config-directory)
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
      (compiled-module-ident
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
                                  (current-config-directory)
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

(define (lua-chunk-mode)
  (current-lua-linker lua-link/chunk)
  (current-lua-runner #'lua-run/chunk))

(define (lua-collection-mode)
  (current-lua-linker lua-link/collection)
  (current-lua-runner #'lua-run/collection))

(lua-chunk-mode)
