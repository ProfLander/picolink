#lang picolink/r-lua

(require (only-in prelude/equality
                  eq?))

(provide %tag
         tag?
         tag)

(define %tag "__tag")

(define tag?
  (λ (v)
    (eq? v %tag)))

(define tag
  (λ (v)
    (#%intrinsic (#%member v __tag))))
