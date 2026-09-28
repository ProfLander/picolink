#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         picolink/config)

(provide (all-from-out racket/base)
         (all-from-out picolink/config)
         #%module-begin
         #%datum)

(define-syntax #%module-begin
  (syntax-parser
    #:datum-literals [define quote]
    [(_ (~seq name:keyword value:expr)
        ...)
     (with-syntax ([lctx #'((~@ name value) ...)])
       #'(#%plain-module-begin
          (provide #%config)
          (define #%config
            (make-config lctx (~@ name value) ...))))]))
