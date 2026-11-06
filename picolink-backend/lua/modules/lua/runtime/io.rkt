#lang picolink/r-lua/base

(require (only lua/io
               stdout
               stderr)

         (only lua/ext/table
               table-ref)

         (only lua/ext/object
               send)

         r-lua/prelude/definition
         r-lua/prelude/print
         r-lua/prelude/void)

(provide (all-defined))

(define (set-unbuffered-stdio!)
  (send stdout "setvbuf" "no")
  (send stderr "setvbuf" "no")
  (void))
