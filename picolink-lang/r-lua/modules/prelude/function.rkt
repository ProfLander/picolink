#lang picolink/r-lua/base

(require prelude/surface)

(provide (all-defined))

(define (identity x)
  x)

(define (const x)
  (λ (_) x))
