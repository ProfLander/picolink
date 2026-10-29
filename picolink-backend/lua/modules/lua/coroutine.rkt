#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member coroutine _)
  create
  resume
  running
  status
  wrap
  yield)
