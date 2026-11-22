#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member (#%member _G os) _)
  clock
  date
  [difftime   diff-time]
  execute
  exit
  [getenv     get-env]
  remove
  rename
  [setlocale  set-locale]
  time
  [tmpname    tmp-name])
