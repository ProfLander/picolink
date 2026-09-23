#lang racket/base

(require racket/string
         racket/port

         syntax/id-set
         syntax/id-table

         picolink/backend
         (prefix-in output: picolink/output)
         picolink/output/s-lua)

(provide (all-defined-out))

(define (module-path->lua-path path)
  (string-replace (path->string path) "/" "."))

(define (module-string->lua-string path)
  (string-replace (symbol->string path) "/" "."))

(define (module-ident->lua-ident path)
  (string->symbol
   (string-replace (symbol->string path) "/" "_")))

(define (splice-requires+provides path output requires provides)
  (s-lua-map
   output

   (λ (body)

     (let ([reqs (for*/fold ([acc null])
                            ([(mod reqs) (in-free-id-table
                                          (hash-ref requires path))])
                   (append acc
                           (list (cons (syntax-e mod)
                                       (free-id-set->list reqs)))))]

           [provs (free-id-set->list (hash-ref provides path))])

       (with-syntax ([(req-mod-sym ...)
                      (for/list ([pair (in-list reqs)])
                        (module-ident->lua-ident (car pair)))]

                     [(req-mod-str ...)
                      (for/list ([pair (in-list reqs)])
                        (module-string->lua-string (car pair)))]

                     [([req-bind-mod . req-bind-sym] ...)
                      (for*/fold ([acc null])
                                 ([pair (in-list reqs)]
                                  [req (in-list (cdr pair))])
                        (cons (cons (module-ident->lua-ident (car pair))
                                    req)
                              acc))]

                     [(prov ...) provs])

         (append
          (if (pair? reqs)
              (list #'(#%local [req-mod-sym ...]
                               [(require req-mod-str) ...])
                    #'(#%local [req-bind-sym ...]
                               [(#%member req-bind-mod
                                          req-bind-sym) ...]))
              null)

          body

          (if (pair? provs)
              (list #'(#%return (#%table [prov prov] ...)))
              null)))))))

(define (lift-package-preloader path output)
  (s-lua-map
   output

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

(define (lua-output-name _self)
  's-lua)

(define (lua-link self ctx)
  (let* ([entry-point (link-ctx-entry-point ctx)]
         [outputs (link-ctx-outputs ctx)]
         [requires (link-ctx-requires ctx)]
         [provides (link-ctx-provides ctx)]

         [outputs
          (for/hash ([(path output) (in-hash outputs)])
            (values path
                    (splice-requires+provides path
                                              output
                                              requires
                                              provides)))]

         [dependencies
          (for/fold ([acc (s-lua null)])
                    ([(path output) (in-hash outputs)]
                     #:when (not (equal? path entry-point)))

            (s-lua-append acc (lift-package-preloader path output)))]

         [entry-point (hash-ref outputs entry-point)]

         [combined (s-lua-append dependencies entry-point)])

    (output:compile combined)))

(define (lua-run _self chunk)
  "Run CHUNK in the system Lua interpreter."

  (define lua (find-executable-path "lua") )

  (unless lua
    (error "unable to locate lua executable"))

  (define-values (sp out in err)
    (subprocess #f #f #f lua "-e" chunk))

  (subprocess-wait sp)

  (case (subprocess-status sp)
    [(0) (display (port->string out))]
    [else (displayln (port->string out))
          (display (port->string err))])

  (close-input-port out)
  (close-output-port in)
  (close-input-port err))

(struct lua ()
  #:transparent
  #:methods gen:backend

  [(define output-name lua-output-name)
   (define link lua-link)
   (define run lua-run)])

(define backend-inst (lua))
