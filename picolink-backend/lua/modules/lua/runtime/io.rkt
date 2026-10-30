#lang picolink/r-lua/base

(require (only lua/io
               stdout
               stderr)

         (only lua/ext/table
               table-ref
               table-call)

         prelude/surface
         prelude/print
         prelude/void)

(define (set-unbuffered-stdio!)
  (table-call stdout "setvbuf" "no")
  (table-call stderr "setvbuf" "no")
  (void))
