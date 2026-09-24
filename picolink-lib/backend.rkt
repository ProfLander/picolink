#lang racket/base

(require racket/generic
         racket/contract
         racket/hash
         racket/set

         syntax/id-set
         syntax/id-table

         picolink/path
         picolink/binding

         (prefix-in language: picolink/language))

(provide (all-defined-out))

(struct compile-ctx (search-paths path))

(define/contract (make-compile-ctx search-paths path)
  (-> (listof (or/c symbol? path?)) path? compile-ctx?)
  (compile-ctx search-paths path))

(define (module-exists? module-path)
  (with-handlers ([exn:fail:filesystem?
                   (λ (_) #f)])
    (module-declared? module-path #t)
    #t))

(define/contract (compile-ctx-load-module ctx)
  (-> compile-ctx? (generic-instance/c language:gen:language))
  (for/fold ([acc #f])
            ([search-path (compile-ctx-search-paths ctx)]
             #:break acc)

    (cond
      [(symbol? search-path)
       (let* ([abs-path (string->symbol
                         (format "~a/~a" search-path
                                 (compile-ctx-path ctx)))])

         (if (module-exists? abs-path)
             (dynamic-require abs-path 'language)
             acc))]

      [(path? search-path)
       (let* ([abs-path (module-path->file-path
                         (build-path search-path
                                     (compile-ctx-path ctx)))])

         (if (file-exists? abs-path)
             (dynamic-require abs-path'language)
             acc))])))

(define (compile-ctx-update ctx
                            #:search-paths
                            [search-paths (compile-ctx-search-paths ctx)]
                            #:path
                            [path (compile-ctx-path ctx)])
  (compile-ctx search-paths
               path))

(struct link-ctx (search-paths path modules requires provides))

(define/contract (make-link-ctx ctx modules requires provides)
  (-> compile-ctx?
      (hash/c path? (generic-instance/c language:gen:language))
      (hash/c path? (hash/c binding? (set/c binding?)))
      (hash/c path? (set/c binding?))
      link-ctx?)

  (link-ctx (compile-ctx-search-paths ctx)
            (compile-ctx-path ctx)
            modules
            requires
            provides))

(define (link-ctx-update ctx
                         #:search-paths
                         [search-paths (link-ctx-search-paths ctx)]
                         #:path
                         [path         (link-ctx-path ctx)]
                         #:modules
                         [modules      (link-ctx-modules ctx)]
                         #:requires
                         [requires     (link-ctx-requires ctx)]
                         #:provides
                         [provides     (link-ctx-provides ctx)])
  (link-ctx search-paths
            path
            modules
            requires
            provides))

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

               (unless (set-member? provs req)
                 (raise-syntax-error
                  'require
                  (format "~a does not provide ~a"
                          target (binding-symbol req))
                  lctx
                  req)))))

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

(define backend-registry
  (make-hash))

(define/contract (register-backend name in)
  (-> symbol?
      (-> (generic-instance/c
           gen:backend
           [output-name (-> backend? symbol?)]
           [compile (or/c (-> backend? compile-ctx? any/c) #f)]
           [link (-> backend? link-ctx? any/c)]
           [run (-> backend? any/c any/c)]))
      void)
  (hash-set! backend-registry name in))

(define/contract (backend name)
  (-> symbol?
      (generic-instance/c
       gen:backend
       [output-name (-> backend? symbol?)]
       [compile (or/c (-> backend? compile-ctx? any/c) #f)]
       [link (-> backend? link-ctx? any/c)]
       [run (-> backend? any/c any/c)]))

  (dynamic-require
   (string->symbol (format "picolink/backend/~a" name))
   'backend-inst))
