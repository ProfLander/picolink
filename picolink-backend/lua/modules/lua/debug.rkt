#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member debug _)
  [getfenv       get-fenv]
  [gethook       get-hook]
  [getinfo       get-info]
  [getlocal      get-local]
  [getmetatable  get-metatable]
  [getregistry   get-registry]
  [getupvalue    get-upvalue]
  [setfenv       set-fenv]
  [sethook       set-hook]
  [setlocal      set-local!]
  [setmetatable  set-metatable!]
  [setupvalue    set-upvalue!]
  traceback
  debug)
