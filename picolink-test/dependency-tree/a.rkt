#lang picolink/lambda

(require (in fst/b
             b))

(require (in fst/c
             c))

(define a
  (λ (x)
    (print "a")
    (c (b x))))

(a #f)
