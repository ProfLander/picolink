#lang racket/base

(require racket/contract
         racket/generic
         racket/struct
         
         picolink/path
         picolink/config
         picolink/language)

(provide (all-defined-out))

(struct compile-ctx (config search-paths)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self)
        'compile-ctx)
      (λ (self)
        (list (compile-ctx-config self)
              (cons 'search-paths
                    (compile-ctx-search-paths self))))))])

(define/contract (make-compile-ctx config search-paths)
  (-> config?
      (listof (or/c symbol? path?))
      compile-ctx?)
  (compile-ctx config search-paths))

(define (module-exists? module-path)
  (with-handlers ([exn:fail:filesystem?
                   (λ (_) #f)])
    (module-declared? module-path #t)
    #t))

(define/contract (compile-ctx-load-module ctx)
  (-> compile-ctx? (generic-instance/c gen:language))

  (define entry-point (compile-ctx-entry-point ctx))

  (define mod
    (for/fold ([acc #f])
              ([search-path (compile-ctx-search-paths ctx)]
               #:break acc)

      (cond
        [(symbol? search-path)
         (let* ([abs-path (string->symbol
                           (format "~a/~a" search-path entry-point))])

           (if (module-exists? abs-path)
               (dynamic-require abs-path 'language)
               acc))]

        [(path? search-path)
         (let* ([abs-path (module-path->file-path
                           (build-path search-path entry-point))])

           (if (file-exists? abs-path)
               (dynamic-require abs-path 'language)
               acc))])))

  (unless mod
    (error (format "module not found: ~a" entry-point)))

  mod)

(define (compile-ctx-project ctx)
  (config-project (compile-ctx-config ctx)))

(define (compile-ctx-root ctx)
  (config-root (compile-ctx-config ctx)))

(define (compile-ctx-entry-point ctx)
  (config-entry-point (compile-ctx-config ctx)))

(define (compile-ctx-backend ctx)
  (config-backend (compile-ctx-config ctx)))

(define (compile-ctx-build-directory ctx)
  (config-build-directory (compile-ctx-config ctx)))

(define (compile-ctx-update ctx
                            #:project
                            [project
                             (compile-ctx-project ctx)]
                            #:root
                            [root
                             (compile-ctx-root ctx)]
                            #:entry-point
                            [entry-point
                             (compile-ctx-entry-point ctx)]
                            #:backend
                            [backend
                             (compile-ctx-backend ctx)]
                            #:build-directory
                            [build-directory
                             (compile-ctx-build-directory ctx)]
                            #:search-paths
                            [search-paths
                             (compile-ctx-search-paths ctx)])

  (compile-ctx (config-update config
                              #:project         project
                              #:root            root
                              #:entry-point     entry-point
                              #:backend         backend
                              #:build-directory build-directory)
               search-paths))
