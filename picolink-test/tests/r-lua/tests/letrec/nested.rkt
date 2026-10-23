#lang picolink/r-lua

(require (only lua
               print))

(letrec ([a (λ (x) (b x))]
         [b identity])

  (print (a 7))

  (letrec ([c (λ (x) (d x))]
           [d a])
    (c 8)))
