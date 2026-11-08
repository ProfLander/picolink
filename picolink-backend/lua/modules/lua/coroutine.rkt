#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member coroutine _)
  create
  resume
  running
  status
  wrap
  yield)
