#lang picolink/r-lua/base

(require (only lua/io  stdout stderr)

         (only lua/ext/object  send)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/void        void))

(provide (all-defined))

(define (set-unbuffered-stdio!)
  (void (send stdout "setvbuf" "no")
        (send stderr "setvbuf" "no")))
