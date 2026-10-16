#lang picolink/r-lua

(require (in lua print)
         (in function identity))

(letrec ([a (λ (x) (b x))]
         [b identity])

  (print (a 8))

  (letrec ([c (λ (x) (d x))]
           [d a])
    (c 9)))
