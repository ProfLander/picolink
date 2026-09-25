#lang racket/base

(require racket/contract
         racket/generic
         racket/set

         picolink/binding
         picolink/backend/compile-ctx
         picolink/language)

(provide (all-defined-out))

(struct link-ctx (search-paths path modules requires provides)
  #:transparent)

(define/contract (make-link-ctx ctx modules requires provides)
  (-> compile-ctx?
      (hash/c path? (generic-instance/c gen:language))
      (hash/c path? (hash/c binding? (set/c (cons/c binding? binding?))))
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
