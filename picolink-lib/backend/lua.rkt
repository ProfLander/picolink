#lang errortrace racket/base

(require racket/contract
         racket/string
         racket/function
         racket/set
         racket/hash
         racket/path
         racket/file
         racket/port
         racket/system

         syntax/parse

         picolink/config
         picolink/backend
         picolink/binding
         picolink/s-lua
         picolink/intrinsic
         (prefix-in language: picolink/language))

(provide (all-defined-out))

(define/contract current-mode
  (parameter/c (or/c 'chunk 'library))
  (make-parameter 'library))

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
                      ([(bind reqs) (in-hash requires)]
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
                 (values req
                         (with-syntax ([mod-intpath mod-intpath]
                                       [req (binding-ident req)])
                           #'(#%member mod-intpath req))))))])

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
                 ([(bind reqs) (in-hash reqs)]

                  #:do [(define mod-path
                          (binding->module-path bind))

                        (define mod
                          (hash-ref (link-ctx-modules ctx)
                                    mod-path #f))]

                  #:when (s-lua? mod))

        (append acc
                (list (cons (binding-symbol bind)
                            (set-map reqs binding-symbol)))))))

  (define (collect-provides ctx)

    (let ([provs (hash-ref (link-ctx-provides ctx)
                           (link-ctx-path ctx))])

      (set-map provs binding-symbol)))

  (define (make-module-bindings reqs)
    (with-syntax ([(req-mod-sym ...)
                   (for/list ([pair (in-list reqs)])
                     (module-ident->lua-ident (car pair)))]

                  [(req-mod-str ...)
                   (for/list ([pair (in-list reqs)])
                     (module-string->lua-string
                      (car pair)))])

      #'(#%local [req-mod-sym ...]
                 [(require req-mod-str) ...])))

  (define (make-member-bindings reqs)
    (with-syntax ([([req-bind-mod . req-bind-sym] ...)
                   (for*/fold ([acc null])
                              ([pair (in-list reqs)]
                               [req (in-list (cdr pair))])
                     (cons (cons (module-ident->lua-ident
                                  (car pair))
                                 req)
                           acc))])
      #'(#%local [req-bind-sym ...]
                 [(#%member req-bind-mod
                            req-bind-sym) ...])))

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
  (s-lua-search-paths))

(define (lua-link self ctx)
  (let* ([ctx
          (link-ctx-with-modules
           ctx
           (for/hash ([(path mod) (in-hash (link-ctx-modules ctx))]
                      #:when (s-lua? mod))
             (values path
                     (rewrite-intrinsics
                      (link-ctx-with-path ctx path)))))]

         [ctx
          (link-ctx-with-modules
           ctx
           (for/hash ([(path mod) (in-hash (link-ctx-modules ctx))]
                      #:when (s-lua? mod))
             (values path
                     (splice-requires+provides
                      (link-ctx-with-path ctx path)))))]

         [mode (current-mode)])

    (case mode
      [(chunk) (lua-link/chunk ctx)]
      [(library) (lua-link/library ctx)]
      [else (error "unsupported mode" mode)])))

(define (lua-link/chunk ctx)
  (let* ([entry-point (link-ctx-path ctx)]

         [dependencies
          (for/fold ([acc (s-lua null)])
                    ([(path mod) (in-hash (link-ctx-modules ctx))]
                     #:when (not (equal? path entry-point)))

            (s-lua-append acc (lift-package-preloader path mod)))]

         [entry-point (hash-ref (link-ctx-modules ctx) entry-point)]

         [combined (s-lua-append dependencies entry-point)])

    (language:compile combined 'lua)))

(define (lua-link/library ctx)
  (let ([modules (for/hash ([(path mod) (in-hash (link-ctx-modules ctx))])
                   (values (path-replace-extension path ".lua")
                           (language:compile mod 'lua)))]
        [build-directory (build-path (current-build-directory) "lua")])

    (make-directory* build-directory)

    (for ([(path mod) (in-hash modules)])
      (let ([path (build-path build-directory path)])
        (make-directory* (path-only path))
        (display-to-file mod path #:exists 'replace)
        (flush-output)))

    (file-name-from-path (link-ctx-path ctx))))

(define (lua-run _self input)
  "Run CHUNK in the system Lua interpreter."

  (define lua (find-executable-path "lua") )

  (unless lua
    (error "unable to locate lua executable"))

  (define-values (sp out in err)
    (let ([mode (current-mode)])
      (case mode
        [(chunk) (lua-run/chunk lua input)]
        [(library) (lua-run/library lua input)])))

  (subprocess-wait sp)

  (case (subprocess-status sp)
    [(0) (display (port->string out))]
    [else (displayln (port->string out))
          (display (port->string err))])

  (close-input-port out)
  (close-output-port in)
  (close-input-port err))

(define (lua-run/chunk lua chunk)
  (subprocess #f #f #f lua "-e" chunk))

(define (lua-run/library lua library)
  (subprocess #f #f #f lua
              "-e"
              (format "package.path = \"~a/\" .. package.path"
                      (build-path (current-build-directory) "lua"))
              "-l"
              library))

(struct lua ()
  #:transparent
  #:methods gen:backend

  [(define (output-name _self)
     (lua-output-name))

   (define (search-paths _self)
     (lua-search-paths))

   (define link lua-link)
   (define run lua-run)])

(define backend-inst (lua))
