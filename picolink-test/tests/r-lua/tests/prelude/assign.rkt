#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/assignment  set!)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)

         r-lua/prelude/number)

(define a 1)
(set! a 2)
(print a)

(define b 3)
(set! b (if #t 4 5))
(print b)
