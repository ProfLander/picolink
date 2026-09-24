#lang racket/base

(require racket/contract
         racket/generic
         
         picolink/path
         picolink/language)

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
  (-> compile-ctx? (generic-instance/c gen:language))
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
