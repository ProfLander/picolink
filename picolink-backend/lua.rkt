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
         picolink/config/parameters
         picolink/s-lua
         picolink/s-lua/lua
         picolink/module/compiled
         picolink/module/import

         (for-template racket/base
                       picolink/s-lua/ir
                       picolink/lua/run))

(provide (all-defined-out)
         (for-template (all-from-out picolink/lua/run)))

#:target       lua-target
#:search-paths lua-search-paths
#:transform    lua-transform
#:link         lua-link
#:run          lua-run
#:interact     lua-interact

(define current-debug
  (make-parameter #t))

(define (lua-search-paths)
  (list 'picolink/lua/modules))

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
               #:when (compiled-module-stx mod))

      (transform-mod lctx prog mod))))

(define (lua-transform-module _lctx prog mod)
  (let* ([is-entry-point (equal? (syntax-e (compiled-module-ident mod))
                                 'entry-point)]
         [mod (splice-requires mod)]
         [mod (splice-lctx mod)]
         [mod (splice-provides mod)]
         [mod (if is-entry-point
                  (splice-unbuffered-stdio mod)
                  mod)]
         [mod (if is-entry-point
                  (splice-repl mod)
                  mod)])

    mod))

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

(define (splice-requires mod)

  (define require-scope (compiled-module-require-scope mod))

  (define reqs
    (reverse
     (for*/list ([(mod-path imports) (in-scope require-scope)]
                 [(_ import)         (in-dict imports)])
       import)))

  (define req-mods
    (reverse (scope-idents require-scope)))

  (define req-mod-syms
    (set-map req-mods module-ident->lua-ident))

  (define req-mod-strs
    (set-map req-mods module-ident->lua-string))

  (define module-bindings
    (with-syntax ([(req-mod-sym ...) req-mod-syms]
                  [(req-mod-str ...) req-mod-strs])

      #'(#%local [req-mod-sym ...]
                 [(#%call require req-mod-str) ...])))

  (define mod-req-binds
    (for*/fold ([acc null])
               ([req (in-list reqs)])
      (let ([src-mod-path (import-src-mod-path req)]
            [from (import-src-sym req)]
            [to (import-local-id req)])
        (cons (list (module-ident->lua-ident src-mod-path)
                    from
                    to)
              acc))))

  (define member-bindings
    (with-syntax ([((req-bind-mod req-bind-from req-bind-to) ...)
                   mod-req-binds])

      #'(#%local [req-bind-to ...]
                 [(#%member req-bind-mod
                            req-bind-from) ...])))

  (let* ([stx (compiled-module-stx mod)]

         [stx (if (pair? mod-req-binds)
                  (s-lua-append
                   (in-s-lua
                    (with-syntax ([member-bindings member-bindings])
                      #'(#%chunk
                         (#%block
                          member-bindings))))
                   stx)
                  stx)]

         [stx (if (pair? req-mod-syms)
                  (s-lua-append
                   (in-s-lua
                    (with-syntax ([module-bindings module-bindings])
                      #'(#%chunk
                         (#%block
                          module-bindings))))
                   stx)
                  stx)])

    (compiled-module-update mod #:stx stx)))

(define (splice-lctx mod)

  (define module-scope
    (compiled-module-module-scope mod))

  (define definitions
    (scope-idents module-scope))

  (let ([stx (compiled-module-stx mod)])

    (compiled-module-update
     mod
     #:stx (s-lua-append
            stx
            (in-s-lua
             (with-syntax ([module-name (module-ident->lua-string
                                         (compiled-module-ident mod))]
                           [(def ...) definitions])
               #'(#%chunk
                  (#%block

                   (#%if (#%not lctx)
                         (#%then
                          (#%block
                           (#%assign [lctx] [(#%table)]))))

                   (#%assign [(#%member lctx module-name)]
                             [(#%call setmetatable
                                      (#%table [def def] ...)
                                      (#%table [__index _G]))])))))))))

(define (splice-provides mod)

  (define provide-scope
    (compiled-module-provide-scope mod))

  (define prov-idents
    (scope-idents provide-scope))

  (if (pair? prov-idents)

      (let ([stx (compiled-module-stx mod)])

        (compiled-module-update
         mod
         #:stx (s-lua-append
                stx
                (in-s-lua
                 (with-syntax ([(prov ...) prov-idents])
                   #'(#%chunk
                      (#%block
                       (#%return (#%table [prov prov] ...)))))))))

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

(define (s-lua-append a b)
  (syntax-parse a
    #:literal-sets [s-lua-ir-literals]
    [(#%chunk (#%block (~and body-a (~not (#%return _))) ...
                       (~optional (#%return ret-a))))

     (syntax-parse b
       #:literal-sets [s-lua-ir-literals]
       [(#%chunk (#%block (~and body-b (~not (#%return _))) ...
                          (~optional (#%return ret-b))))
        (when (and (attribute ret-a)
                   (attribute ret-b))
          (error "cannot append s-lua chunks with multiple returns"))

        (in-s-lua
         (with-syntax ([(ret ...) (cond
                                    [(attribute ret-a)
                                     (list #'(#%return ret-a))]
                                    [(attribute ret-b)
                                     (list #'(#%return ret-b))]
                                    [else null])])
           #'(#%chunk
              (#%block
               body-a ...
               body-b ...
               ret ...))))])]))

(define stx-unbuffered-stdio
  (in-s-lua
   #'(#%chunk
      (#%block

       (#%call (#%method (#%member io stdout)
                         setvbuf)
               "no")

       (#%call (#%method (#%member io stderr)
                         setvbuf)
               "no")))))

(define (splice-unbuffered-stdio mod)
  (compiled-module-update
   mod
   #:stx (s-lua-append (compiled-module-stx mod)
                       stx-unbuffered-stdio)))

(define (make-repl-stx mod)
  (with-syntax ([module-name (module-ident->lua-string
                              (compiled-module-ident mod))])
    (in-s-lua
     #'(#%chunk
        (#%block

         (#%local [repl-scope] [module-name])

         (#%while
          #t
          (#%block
           (#%local [input]   [(#%call (#%member io read))])
           (#%local [res err] [(#%call loadstring
                                       input
                                       "repl")])
           (#%if res

                 (#%then
                  (#%block
                   (#%call
                    (#%call setfenv res
                            (#%member lctx
                                      'repl-scope)))))

                 (#%else
                  (#%block
                   (#%call
                    (#%method (#%member io stderr)
                              write)
                    err)))))))))))

(define (splice-repl mod)
  (compiled-module-update
   mod
   #:stx
   (s-lua-append (compiled-module-stx mod)
                 (make-repl-stx mod))))

(define (program-entry-point prog)
  (findf (λ (mod)
           (equal? 'entry-point
                   (syntax-e
                    (compiled-module-ident mod))))
         prog))

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

        [build-directory (build-path (current-build-directory)
                                     (current-lua-build-subdir))])

    (delete-directory/files build-directory #:must-exist? #f)
    (make-directory* build-directory)

    (for ([(path mod) (in-hash modules)])

      (when (current-debug)
        (displayln "~a:\n" path)
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
