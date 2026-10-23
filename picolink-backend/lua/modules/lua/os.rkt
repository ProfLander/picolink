#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
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
