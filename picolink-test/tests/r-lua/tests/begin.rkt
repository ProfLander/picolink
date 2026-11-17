#lang picolink/r-lua/base

(require (only r-lua/runtime/print       print)
         (only r-lua/prelude/sequencing  begin)

         r-lua/prelude/boolean)

(begin
  #f
  (print #t)
  #f)
