#lang picolink/r-lua/base

(provide (all-defined))

(define eq?
  (λ (a b)
    (#%intrinsic (#%eq a b))))
