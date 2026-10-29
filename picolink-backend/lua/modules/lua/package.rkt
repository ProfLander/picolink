#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member package _)
  cpath
  loaded
  loaders
  loadlib
  path
  preload
  seeall)
