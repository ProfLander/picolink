#lang picolink/r-lua/base

(provide (all-defined))

(define identity
  (λ (x) x))

(define const
  (λ (x)
    (λ (_) x)))
