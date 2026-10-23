#lang picolink/r-lua/base

(provide (all-defined))

(define %and
  (λ (a b)
    (#%intrinsic (#%and a b))))

(define-macro and
  [(_ a b rest ...+)
   #'(let ([res (and a b)])
       (and res rest ...))]
  [(_ a b)
   #'(%and a b)])

(define %or
  (λ (a b)
    (#%intrinsic (#%or a b))))

(define-macro or
  [(_ a b rest ...+)
   #'(let ([res (or a b)])
       (or res rest ...))]
  [(_ a b)
   #'(%or a b)])

(define not
  (λ (v)
    (#%intrinsic (#%not v))))

