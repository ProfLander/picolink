#lang picolink/r-lua

(require (in prelude/function
             identity))

(letrec ([a (λ (x) (b x))]
         [b identity])
  (a 7))
