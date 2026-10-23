#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member string _)
  byte
  char
  dump
  find
  format
  gmatch
  gsub
  len
  lower
  match
  rep
  reverse
  sub
  upper)
