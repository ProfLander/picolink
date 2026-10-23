#lang picolink/r-lua/base

(provide string-append)

(define %cat
  (λ (a b)
    (#%intrinsic (#%cat a b))))

(define-macro string-append
  [(_ a b rest ...+)
   #'(let ([cat (string-append a b)])
       (string-append cat rest ...))]
  [(_ a b)
   #'(%cat a b)])
