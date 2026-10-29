#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member debug _)
  debug
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
  traceback)
