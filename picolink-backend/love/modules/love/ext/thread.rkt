#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics

  #:template (#%member (#%member love thread) _)

  Channel
  Thread

  [getChannel get-channel]
  [newChannel new-channel]
  [newThread  new-thread])
