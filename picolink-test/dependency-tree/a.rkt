#lang picolink/input/lambda

(require (in fst/b
             b)
         (in fst/c
             c))

(define a (λ (x) (b (c x))))

a
