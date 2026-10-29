#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member table _)
  concat
  insert
  maxn
  remove
  sort)
