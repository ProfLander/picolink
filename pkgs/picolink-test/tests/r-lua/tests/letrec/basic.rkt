#lang picolink/r-lua/base

(require (only r-lua/prelude/binding   letrec)
         (only r-lua/prelude/lambda    λ)
         (only r-lua/prelude/function  identity)

         r-lua/prelude/number)

(letrec ([a (λ (x) (b x))]
         [b identity])
  (a 6))
