#lang picolink/r-lua

(provide (all-defined))

(define eq?
  (λ (a b)
    (#%intrinsic (#%eq a b))))
