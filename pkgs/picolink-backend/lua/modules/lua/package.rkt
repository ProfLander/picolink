#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member (#%member _G package) _)
  [cpath    c-path]
  loaded
  loaders
  [loadlib  load-lib]
  path
  preload
  [seeall   see-all])
