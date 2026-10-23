#lang picolink/r-lua

(letrec ([a (λ (x) (b x))]
         [b identity])
  (a 6))
