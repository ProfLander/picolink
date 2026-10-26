#lang picolink/r-lua/base

(require prelude/surface)

(provide (all-defined))

(define (member t k)
  (#%intrinsic (#%member t (quote k))))
