#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define (identity x)
  x)

(define (const x)
  (λ (_) x))

(define-macro thunk
  [(_ body ...)
   #'(λ ()
       body ...)])
