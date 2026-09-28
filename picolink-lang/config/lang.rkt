#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/config

         (only-in picolink/racket
                  [make-racket racket])

         (only-in picolink/lua
                  [make-lua lua])

         (only-in picolink/love
                  [make-love love]))

(provide (all-from-out racket/base)
         (all-from-out picolink/config)
         #%module-begin
         #%datum
         racket
         lua
         love)

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
