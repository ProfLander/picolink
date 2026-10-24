#lang picolink/r-lua/base

(require (only lua type)
         prelude/equality)

(provide (all-defined))

(define string?
  (λ (v)
    (eq? (type v) "string")))

(define %cat
  (λ (a b)
    (#%intrinsic (#%cat a b))))

(define-macro string-append
  [(_ a b rest ...+)
   #'(let ([cat (string-append a b)])
       (string-append cat rest ...))]
  [(_ a b)
   #'(%cat a b)])
