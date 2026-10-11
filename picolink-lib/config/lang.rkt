#lang racket/base

(require (for-syntax racket/base
                     racket/path
                     syntax/parse)
         picolink/config)

(provide (all-from-out racket/base)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ (~or (~optional (~seq #:backend         backend))
             (~optional (~seq #:build-directory build-directory))
             body)
        ...)

     #:fail-unless (attribute backend)
     "configs must define #:backend"

     (with-syntax ([root (path-only (syntax-source this-syntax))]
                   [(build-directory ...) (if (attribute build-directory)
                                              (list #'#:build-directory
                                                    #'build-directory)
                                              null)])
       #'(#%plain-module-begin
          body ...

          (provide #%config)
          (define #%config
            (make-config #:root    root
                         #:backend (quote-syntax backend)
                         build-directory ...))))]))
