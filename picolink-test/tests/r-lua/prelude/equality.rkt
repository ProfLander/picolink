#lang picolink/r-lua

(provide eq?)

(define eq?
  (λ (a b)
    (#%intrinsic (#%eq a b))))
