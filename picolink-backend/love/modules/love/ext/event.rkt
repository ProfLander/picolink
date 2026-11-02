#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics

  #:template (#%member (#%member love event) _)

  clear
  poll
  pump
  push
  quit
  wait)
