#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member (#%member _G table) _)
  concat
  insert
  maxn
  remove
  sort)
