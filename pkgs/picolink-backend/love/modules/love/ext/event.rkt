#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics

  #:template (#%member (#%member (#%member _G love) event) _)

  clear
  poll
  pump
  push
  quit
  wait)
