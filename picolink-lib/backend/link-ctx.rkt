#lang racket/base

(require racket/contract
         racket/generic
         racket/struct

         picolink/backend/compile-ctx
         picolink/language)

(provide (all-defined-out))

(struct link-ctx (compile-ctx requires provides modules)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self)
        'link-ctx)
      (λ (self)

        (define (hash->list* h)
          (hash-map h
                    (λ (k v)
                      (cons (string->symbol (path->string k)) v))))

        (list (link-ctx-compile-ctx self)
              (cons 'requires
                    (hash->list* (link-ctx-requires self)))
              (cons 'provides
                    (hash->list* (link-ctx-provides self)))
              (cons 'modules
                    (hash->list* (link-ctx-modules self)))))))])

(define/contract (make-link-ctx compile-ctx requires provides modules)
  (-> compile-ctx?
      (hash/c path? module-requires?)
      (hash/c path? module-provides?)
      (hash/c path? (generic-instance/c gen:language))
      link-ctx?)

  (link-ctx compile-ctx
            requires
            provides
            modules))

(define (link-ctx-config ctx)
  (compile-ctx-config (link-ctx-compile-ctx ctx)))

(define (link-ctx-search-paths ctx)
  (compile-ctx-search-paths (link-ctx-compile-ctx ctx)))

(define (link-ctx-path ctx)
  (compile-ctx-path (link-ctx-compile-ctx ctx)))

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
  (link-ctx (compile-ctx-update (link-ctx-compile-ctx ctx)
                                #:search-paths search-paths
                                #:path path)
            requires
            provides
            modules))
