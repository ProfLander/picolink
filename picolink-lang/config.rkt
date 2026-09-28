#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)

         racket/contract
         racket/generic
         racket/path
         racket/struct

         (only-in picolink/backend
                  gen:backend
                  [make-lua lua]
                  [make-love love]
                  [make-racket racket]))

(provide (all-from-out picolink/backend)
         (all-defined-out)
         file-name-from-path)

(struct config [project
                root
                entry-point
                backend
                build-directory]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (_self) 'config)
      (λ (self)
        (list (cons 'project
                    (config-project self))
              (cons 'root
                    (config-root self))
              (cons 'entry-point
                    (config-entry-point self))
              (cons 'backend
                    (config-backend self))
              (cons 'build-directory
                    (config-build-directory self))))))])

(define/contract (%make-config project
                               root
                               entry-point
                               backend
                               build-directory)
  (-> symbol?
      path-string?
      path-string?
      (generic-instance/c gen:backend)
      path-string?
      config?)

  (config project
          root
          entry-point
          backend
          build-directory))

(define-syntax make-config
  (syntax-parser
    [(_ lctx:expr
        (~alt (~optional (~seq #:project project:id))
              (~optional (~seq #:entry-point entry-point:id)
                         #:defaults ([entry-point #'main]))
              (~optional (~seq #:backend backend:expr))
              (~optional (~seq #:build-directory build-directory:string)
                         #:defaults ([build-directory #'"build"])))
        ...)

     (unless (attribute backend)
       (raise-syntax-error 'config
                           "missing required parameter `#:backend`"
                           #'lctx))

     (with-syntax* ([root (syntax-local-identifier-as-binding
                           (datum->syntax this-syntax 'root))]
                    [project
                     (if (attribute project)
                         #''project
                         #'(let*-values ([(_base proj _must-be-dir?)
                                          (split-path root)])

                             (string->symbol
                              (path->string proj))))])

       #'(let ([path (variable-reference->module-source
                      (#%variable-reference))])

           (let-values ([(root _name _must-be-dir?)
                         (split-path path)])

             (%make-config project
                           root
                           (string->path (symbol->string 'entry-point))
                           backend
                           (string->path build-directory)))))]))

(define (config-update base
                       #:project
                       [project         (config-project base)]
                       #:root
                       [root            (config-root base)]
                       #:entry-point
                       [entry-point     (config-entry-point base)]
                       #:backend
                       [backend         (config-backend base)]
                       #:build-directory
                       [build-directory (config-build-directory base)])
  (->* [config?]
       [#:project         symbol?
        #:root            path-string?
        #:entry-point     path-string?
        #:backend         (generic-instance/c gen:backend)
        #:build-directory path-string?]
       config?)

  (config project
          root
          entry-point
          backend
          build-directory))

(define (get-config path)
  (let ([path (build-path path "config.rkt")])
    (and (file-exists? path)
         (dynamic-require path '#%config))))

(define (find-config [path (current-directory)])
  (let loop ([path (simplify-path (path->complete-path path))])
    (and path
         (let ([config (get-config path)])
           (if config
               config
               (let-values ([(base _name _must-be-dir?)
                             (split-path path)])
                 (loop base)))))))
