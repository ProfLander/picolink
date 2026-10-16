#lang picolink/r-lua

(require (in function identity))

(letrec ([a (λ (x) (b x))]
         [b identity])
  (a 7))
