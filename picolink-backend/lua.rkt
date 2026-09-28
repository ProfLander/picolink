#lang racket/base

(require racket/contract
         racket/string
         racket/path
         racket/set
         racket/hash
         racket/function
         racket/file
         racket/port

         syntax/parse

         picolink/parameters
         picolink/backend/generic
         picolink/backend/link-ctx
         (only-in picolink/language
                  [compile language:compile])
         picolink/language/requires
         picolink/language/provides
         picolink/binding
         picolink/intrinsic
         picolink/s-lua)

(provide (all-defined-out))

(define (module-path->lua-path path)
  (string-replace (path->string path) "/" "."))

(define (module-string->lua-string path)
  (string-replace (symbol->string path) "/" "."))

(define (module-ident->lua-ident path)
  (string->symbol
   (string-replace (symbol->string path) "/" "_")))

(define (binding->module-path bind)
  (string->path
   (symbol->string
    (binding-symbol bind))))

(define (rewrite-intrinsics ctx)

  (define (rewrite-body ctx body)

    (let* ([path (link-ctx-path ctx)]
           [requires (hash-ref (link-ctx-requires ctx) path)]
           [rewrites
            (for/fold ([acc (hash)])
                      ([(bind reqs) (in-module-requires requires)]
                       #:do [(define mod-path
                               (binding->module-path bind))
                             (define mod
                               (hash-ref (link-ctx-modules ctx)
                                         mod-path))]

                       #:when (intrinsic? mod)

                       #:do [(define mod-intpath (intrinsic-path mod))]

                       #:when mod-intpath)

              (hash-union
               acc
               (for/hash ([req (in-set reqs)])
                 (let ([from (module-require-from req)]
                       [to (module-require-to req)])
                   (values
                    to
                    (with-syntax
                      ([mod-intpath
                        (case mod-intpath
                          [(same)
                           (let ([segs (map (compose string->symbol
                                                     path->string)
                                            (explode-path mod-path))])
                             (for/fold ([acc (car segs)])
                                       ([seg (in-list
                                              (cdr segs))])
                               #`(#%member #,acc #,seg)))]

                          [else mod-intpath])]

                       [from (binding-ident from)])

                      #'(#%member mod-intpath from)))))))])

      (define rewrite
        (syntax-parser
          [(expr ...)
           (datum->syntax this-syntax (map rewrite (attribute expr)))]

          [ident:id

           #:do [(define rewrite
                   (hash-ref rewrites
                             (make-binding #'ident)
                             #f))]

           #:when rewrite

           rewrite]

          [_ this-syntax]))

      (map rewrite body)))

  (let ([mod (hash-ref (link-ctx-modules ctx)
                       (link-ctx-path ctx))])
    (s-lua-map mod (curry rewrite-body ctx))))

(define (splice-requires+provides ctx)

  (define (collect-requires ctx)

    (let ([reqs (hash-ref (link-ctx-requires ctx)
                          (link-ctx-path ctx))])

      (for*/fold ([acc null])
                 ([(bind reqs) (in-module-requires reqs)]

                  #:do [(define mod-path
                          (binding->module-path bind))

                        (define mod
                          (hash-ref (link-ctx-modules ctx)
                                    mod-path #f))]

                  #:when (s-lua? mod))

        (append acc
                (list (cons (binding-symbol bind)
                            (set-map
                             reqs
                             (λ (req)
                               (cons
                                (binding-symbol
                                 (module-require-from req))
                                (binding-symbol
                                 (module-require-to req)))))))))))

  (define (collect-provides ctx)

    (let ([provs (hash-ref (link-ctx-provides ctx)
                           (link-ctx-path ctx))])

      (module-provides-map provs binding-symbol)))

  (define (make-module-bindings reqs)
    (with-syntax ([(req-mod-sym ...)
                   (for/list ([pair (in-list reqs)])
                     (module-ident->lua-ident (car pair)))]

                  [(req-mod-str ...)
                   (for/list ([pair (in-list reqs)])
                     (module-string->lua-string
                      (car pair)))])

      #'(#%local [req-mod-sym ...]
                 [(#%call require req-mod-str) ...])))

  (define (make-member-bindings reqs)
    (with-syntax ([((req-bind-mod req-bind-from req-bind-to) ...)
                   (for*/fold ([acc null])
                              ([pair (in-list reqs)]
                               [req (in-list (cdr pair))])
                     (let ([from (car req)]
                           [to (cdr req)])
                       (cons (list(module-ident->lua-ident
                                   (car pair))
                                  from
                                  to)
                             acc)))])
      #'(#%local [req-bind-to ...]
                 [(#%member req-bind-mod
                            req-bind-from) ...])))

  (define (make-provide-return provs)
    (with-syntax ([(prov ...) provs])
      #'(#%return (#%table [prov prov] ...))))

  (define (splice-body ctx body)
    (let* ([reqs (collect-requires ctx)]
           [provs (collect-provides ctx)])

      (append
       (if (pair? reqs)
           (list (make-module-bindings reqs)
                 (make-member-bindings reqs))
           null)

       body

       (if (pair? provs)
           (list (make-provide-return provs))

           null))))

  (let ([mod (hash-ref (link-ctx-modules ctx)
                       (link-ctx-path ctx))])
    (s-lua-map mod (curry splice-body ctx))))

(define (lift-package-preloader path module)
  (s-lua-map
   module

   (λ (body)

     (with-syntax ([(body ...) body]
                   [path (module-path->lua-path path)])

       (list #'(#%assign
                [(#%member (#%member package preload)
                           path)]
                [(#%function
                  (#%vararg)
                  (#%block
                   body ...))]))))))

(define (lua-output-name)
  's-lua)

(define (lua-search-paths)
  (list 'picolink/lua/modules))

(define (lua-link self ctx)

  (let* ([ctx
          (link-ctx-update
           ctx
           #:modules
           (for/hash ([(path mod) (in-hash (link-ctx-modules ctx))]
                      #:when (s-lua? mod))
             (values path
                     (rewrite-intrinsics
                      (link-ctx-update ctx #:path path)))))]

         [ctx
          (link-ctx-update
           ctx
           #:modules
           (for/hash ([(path mod) (in-hash (link-ctx-modules ctx))]
                      #:when (s-lua? mod))
             (values path
                     (splice-requires+provides
                      (link-ctx-update ctx #:path path)))))])

    (let ([mode (lua-mode self)])
      (case mode
        [(chunk) (lua-link/chunk self ctx)]
        [(library) (lua-link/library self ctx)]
        [else (error "unsupported mode" mode)]))))

(define (lua-link/chunk self ctx)
  (let* ([entry-point (link-ctx-path ctx)]

         [dependencies
          (for/fold ([acc (s-lua #'(#%chunk (#%block)) null)])
                    ([(path mod) (in-hash (link-ctx-modules ctx))]
                     #:when (not (equal? path entry-point)))

            (s-lua-append acc (lift-package-preloader path mod)))]

         [entry-point (hash-ref (link-ctx-modules ctx) entry-point)]

         [combined (s-lua-append dependencies entry-point)])

    (language:compile combined 'lua)))

(define (lua-link/library self ctx)
  (let ([modules (for/hash ([(path mod) (in-hash (link-ctx-modules ctx))])
                   (values (path-replace-extension path ".lua")
                           (language:compile mod 'lua)))]
        [build-directory (build-path (current-build-directory)
                                     (lua-build-subdir self))])

    (delete-directory/files build-directory #:must-exist? #f)
    (make-directory* build-directory)

    (for ([(path mod) (in-hash modules)])
      (let ([path (build-path build-directory path)])
        (make-directory* (path-only path))
        (display-to-file mod path #:exists 'replace)
        (flush-output)))

    (file-name-from-path (link-ctx-path ctx))))

(define (lua-run self input)
  "Run CHUNK in the system Lua interpreter."

  (define exe (find-executable-path (lua-executable self)) )

  (unless exe
    (error "unable to locate lua executable"))

  (define-values (sp out in err)
    (case (lua-mode self)
      [(chunk) (lua-run/chunk self exe input)]
      [(library) (lua-run/library self exe input)]))

  (subprocess-wait sp)

  (case (subprocess-status sp)
    [(0) (display (port->string out))]
    [else (displayln (port->string out))
          (display (port->string err))])

  (close-input-port out)
  (close-output-port in)
  (close-input-port err))

(define (lua-run/chunk self exe chunk)
  (subprocess #f #f #f exe "-e" chunk))

(define (lua-run/library self exe library)
  (subprocess #f #f #f exe
              "-e"
              (format "package.path = \"~a/\" .. package.path"
                      (build-path (current-build-directory)
                                  (lua-build-subdir self)))
              "-l"
              library))

(struct lua (mode build-subdir executable)
  #:transparent
  #:methods gen:backend

  [(define (output-name _self)
     (lua-output-name))

   (define (search-paths _self)
     (lua-search-paths))

   (define link lua-link)
   (define run lua-run)])

(define/contract (make-lua #:mode         [mode 'chunk]
                           #:build-subdir [build-subdir "lua"]
                           #:executable   [executable "lua"])
  (->* []
       [#:mode         (or/c 'chunk 'library)
        #:build-subdir path-string?
        #:executable   string?]
       lua?)

  (lua mode
       build-subdir
       executable))
