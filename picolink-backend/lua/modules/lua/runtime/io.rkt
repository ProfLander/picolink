#lang picolink/r-lua/base

(require (only lua/io
               stdout
               stderr)

         (only lua/ext/table
               table-ref
               table-call)

         r-lua/prelude/surface
         r-lua/prelude/print
         r-lua/prelude/void)

(define (set-unbuffered-stdio!)
  (table-call stdout "setvbuf" "no")
  (table-call stderr "setvbuf" "no")
  (void))
