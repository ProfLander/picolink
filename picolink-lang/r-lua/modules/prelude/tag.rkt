#lang picolink/r-lua/base

(require (only prelude/equality
               eq?))

(provide (all-defined))

(define %tag "__tag")

(define tag?
  (λ (v)
    (eq? v %tag)))

(define tag
  (λ (v)
    (#%intrinsic (#%member v __tag))))
