#lang errortrace racket/base

(require racket/generic
         racket/hash
         racket/set

         (prefix-in language: picolink/language)
         picolink/language
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
                                    (language:requires mod))])

               (for*/fold ([modules (hash-set modules path mod)]
                           [requires (hash-union requires mod-reqs)])
                          ([(_ reqs) (in-hash mod-reqs)]
                           [(mod _) (in-module-requires reqs)])

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
                         (values path (language:provides mod)))])

         (for* ([(_ reqs) (in-hash requires)]
                [(target reqs) (in-module-requires reqs)])

           (let* ([target (string->path
                           (symbol->string
                            (binding-symbol target)))]
                  [provs (hash-ref provides target)])

             (for ([req (in-set reqs)])

               (let ([from (module-require-from req)])
                 (unless (module-provides-member? provs from)
                   (raise-syntax-error
                    'require
                    (format "~a does not provide ~a"
                            target (binding-symbol from))
                    (module-require-lctx req)
                    from))))))

         (let ([modules
                (for/hash ([(path mod) (in-hash modules)]
                           #:do [(define compiled
                                   (language:compile
                                    mod
                                    (call-output-name backend)))]
                           #:when compiled)

                  (values path compiled))])

           (make-link-ctx
            ctx
            modules
            requires
            provides)))))])
