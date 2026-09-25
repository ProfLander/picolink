#lang racket/base

(require racket/contract
         racket/generic
         
         picolink/path
         picolink/language)

(provide (all-defined-out))

(struct compile-ctx (search-paths path)
  #:transparent)

(define/contract (make-compile-ctx search-paths path)
  (-> (listof (or/c symbol? path?)) path? compile-ctx?)
  (compile-ctx search-paths path))

(define (module-exists? module-path)
  (with-handlers ([exn:fail:filesystem?
                   (λ (_) #f)])
    (module-declared? module-path #t)
    #t))

(define/contract (compile-ctx-load-module ctx)
  (-> compile-ctx? (generic-instance/c gen:language))

  (define path (compile-ctx-path ctx))

  (define mod
    (for/fold ([acc #f])
              ([search-path (compile-ctx-search-paths ctx)]
               #:break acc)

      (cond
        [(symbol? search-path)
         (let* ([abs-path (string->symbol
                           (format "~a/~a" search-path path))])

           (if (module-exists? abs-path)
               (dynamic-require abs-path 'language)
               acc))]

        [(path? search-path)
         (let* ([abs-path (module-path->file-path
                           (build-path search-path path))])

           (if (file-exists? abs-path)
               (dynamic-require abs-path 'language)
               acc))])))

  (unless mod
    (error (format "module not found: ~a" path)))

  mod)

(define (compile-ctx-update ctx
                            #:search-paths
                            [search-paths (compile-ctx-search-paths ctx)]
                            #:path
                            [path (compile-ctx-path ctx)])
  (compile-ctx search-paths
               path))
