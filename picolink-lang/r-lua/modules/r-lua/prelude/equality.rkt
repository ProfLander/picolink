#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define (eq? a b)
  (s-lua (#%eq a b)))
