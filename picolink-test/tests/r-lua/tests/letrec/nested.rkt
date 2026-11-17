#lang picolink/r-lua

(require (only r-lua/runtime/print     print)

         (only r-lua/prelude/binding   letrec)
         (only r-lua/prelude/lambda    λ)
         (only r-lua/prelude/function  identity)

         r-lua/prelude/number)

(letrec ([a (λ (x) (b x))]
         [b identity])

  (print (a 7))

  (letrec ([c (λ (x) (d x))]
           [d a])
    (c 8)))
