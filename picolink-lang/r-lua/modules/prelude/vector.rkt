#lang picolink/r-lua/base

(provide (all-defined))

(define-macro %vector
  [(_ v ...)
   (with-syntax ([(k ...) (map (λ (_) (generate-temporary))
                               (attribute v))])
     #'(let ([k v]
             ...)
         (s-lua (#%table v ...))))])
