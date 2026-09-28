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

(define (link-ctx-project ctx)
  (compile-ctx-project (link-ctx-compile-ctx ctx)))

(define (link-ctx-root ctx)
  (compile-ctx-root (link-ctx-compile-ctx ctx)))

(define (link-ctx-entry-point ctx)
  (compile-ctx-entry-point (link-ctx-compile-ctx ctx)))

(define (link-ctx-backend ctx)
  (compile-ctx-backend (link-ctx-compile-ctx ctx)))

(define (link-ctx-build-directory ctx)
  (compile-ctx-build-directory (link-ctx-compile-ctx ctx)))

(define (link-ctx-search-paths ctx)
  (compile-ctx-search-paths (link-ctx-compile-ctx ctx)))

(define (link-ctx-update ctx
                         #:project
                         [project         (link-ctx-project ctx)]
                         #:root
                         [root            (link-ctx-root ctx)]
                         #:entry-point
                         [entry-point     (link-ctx-entry-point ctx)]
                         #:backend
                         [backend         (link-ctx-backend ctx)]
                         #:build-directory
                         [build-directory (link-ctx-build-directory ctx)]
                         #:search-paths
                         [search-paths    (link-ctx-search-paths ctx)]
                         #:requires
                         [requires        (link-ctx-requires ctx)]
                         #:provides
                         [provides        (link-ctx-provides ctx)]
                         #:modules
                         [modules         (link-ctx-modules ctx)])
  (link-ctx (compile-ctx-update (link-ctx-compile-ctx ctx)
                                #:project         project
                                #:root            root
                                #:entry-point     entry-point
                                #:backend         backend
                                #:build-directory build-directory
                                #:search-paths    search-paths)
            requires
            provides
            modules))
