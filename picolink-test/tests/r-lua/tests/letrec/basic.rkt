#lang picolink/r-lua

(require (only prelude/function
               identity))

(letrec ([a (λ (x) (b x))]
         [b identity])
  (a 6))
