#lang picolink/r-lua/base

(require (only lua type)
         prelude/equality
         prelude/surface)

(provide (all-defined))

(define (string? v)
  (eq? (type v) "string"))

(define (%cat a b)
  (#%intrinsic (#%cat a b)))

(define-macro string-append
  [(_ a b rest ...+)
   #'(let ([cat (string-append a b)])
       (string-append cat rest ...))]
  [(_ a b)
   #'(%cat a b)])
