#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member debug _)
  getfenv
  gethook
  getinfo
  getlocal
  getmetatable
  getregistry
  getupvalue
  setfenv
  sethook
  setlocal
  setmetatable
  setupvalue
  traceback
  debug)
