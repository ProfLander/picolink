#lang picolink/r-lua/base

(provide (all-defined))

(define member
  (λ (t k)
    (#%intrinsic (#%member t (quote k)))))
