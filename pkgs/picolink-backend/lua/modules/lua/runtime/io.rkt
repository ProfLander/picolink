#lang picolink/r-lua/base

(require (only lua/io  stdout stderr)

         (only lua/ext/ops  :)

         (only r-lua/runtime/void        void)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (set-unbuffered-stdio!)
  (void (: stdout "setvbuf" "no")
        (: stderr "setvbuf" "no")))
