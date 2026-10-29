#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics
  #:template (#%member io _)
  close
  flush
  input
  lines
  open
  output
  popen
  read
  tmpfile
  type
  write

  stdin
  stdout
  stderr)

;file:close
;file:flush
;file:lines
;file:read
;file:seek
;file:setvbuf
;file:write
