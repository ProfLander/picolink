#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member (#%member _G coroutine) _)
  create
  resume
  running
  status
  wrap
  yield)
