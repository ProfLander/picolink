#lang picolink/r-lua

(provide %vector)

(define-macro %vector
  [(_ v ...)
   (with-syntax ([(k ...) (map (λ (_) (generate-temporary))
                               (attribute v))])
     #'(let ([k v]
             ...)
         (#%intrinsic (#%table v ...))))])
