#lang picolink/r-lua/base

(require r-lua/prelude/definition
         r-lua/prelude/s-lua)

(provide (all-defined))

(define (eq? a b)
  (s-lua (#%eq a b)))
