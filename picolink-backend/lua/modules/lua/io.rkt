#lang picolink/r-lua/base

(require (only intrinsic  define-intrinsics))

(provide (all-defined))

(define-intrinsics
  #:template (#%member (#%member _G io) _)

  close
  flush
  input
  lines
  open
  output
  popen
  read
  [tmpfile  tmp-file]
  type
  write

  stdin
  stdout
  stderr)
