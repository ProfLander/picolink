#lang picolink/r-lua/base

(require (only lua/io  stdout stderr)

         (only lua/ext/object  send)

         (only r-lua/runtime/void        void)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (set-unbuffered-stdio!)
  (void (send stdout "setvbuf" "no")
        (send stderr "setvbuf" "no")))
