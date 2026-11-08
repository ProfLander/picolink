#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member os _)
  clock
  date
  difftime
  execute
  exit
  getenv
  remove
  rename
  setlocale
  time
  tmpname)
