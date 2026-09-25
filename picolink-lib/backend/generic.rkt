#lang racket/base

(require racket/generic
         racket/hash
         racket/set

         (prefix-in language: picolink/language)
         picolink/binding
         picolink/backend/compile-ctx
         picolink/backend/link-ctx)

(provide (all-defined-out))

(define-generics backend
  (output-name backend)
  (search-paths backend)

  (compile backend ctx)
  (link backend ctx)
  (run backend linked)

  #:fallbacks
  [(define/generic call-output-name output-name)
   (define/generic call-link link)
   (define/generic call-run run)

   (define (search-paths backend)
     null)

   (define (compile backend ctx)

     (define (collect-module ctx modules requires)

       (let ([path (compile-ctx-path ctx)])
         (if (hash-has-key? modules path)

             (values modules requires)

             (let* ([mod      (compile-ctx-load-module ctx)]
                    [mod-reqs (hash path
                                    (language:collect-requires mod))])

               (for*/fold ([modules (hash-set modules path mod)]
                           [requires (hash-union requires mod-reqs)])
                          ([(_ reqs) (in-hash mod-reqs)]
                           [(_ reqs) (in-hash reqs)]
                           [(mod _) (in-hash reqs)])

                 (collect-module
                  (compile-ctx-update
                   ctx
                   #:path
                   (string->path
                    (symbol->string
                     (binding-symbol mod))))
                  modules requires))))))

     (let-values ([(modules requires)
                   (collect-module ctx (hash) (hash))])

       (let ([provides (for/hash ([(path mod) (in-hash modules)])
                         (values path (language:collect-provides mod)))])

         (for* ([(_ reqs) (in-hash requires)]
                [(lctx reqs) (in-hash reqs)]
                [(target reqs) (in-hash reqs)])

           (let* ([target (string->path
                           (symbol->string
                            (binding-symbol target)))]
                  [provs (hash-ref provides target)])

             (for ([req (in-set reqs)])

               (let ([from (car req)])
                 (unless (set-member? provs from)
                   (raise-syntax-error
                    'require
                    (format "~a does not provide ~a"
                            target (binding-symbol from))
                    lctx
                    from))))))

         (let ([modules
                (for/hash ([(path mod) (in-hash modules)]
                           #:do [(define compiled
                                   (language:compile
                                    mod
                                    (call-output-name backend)))]
                           #:when compiled)

                  (values path compiled))]

               [requires
                (for*/fold ([acc
                             (for/hash ([(path _) (in-hash requires)])
                               (values
                                path
                                (hash)))])
                           ([(path reqs) (in-hash requires)]
                            [(_ req-tbl) (in-hash reqs)]
                            [(mod req-set) (in-hash req-tbl)])
                  (hash-update
                   acc path
                   (λ (tgt)
                     (hash-update
                      tgt mod
                      (λ (tgt)
                        (set-union tgt req-set))
                      req-set))
                   req-tbl))])

           (make-link-ctx
            ctx
            modules
            requires
            provides)))))])
