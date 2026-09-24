#lang picolink/lambda

(require (in lua print)
         (in fst/b b)
         (in fst/c c))

(define a
  (λ (x)
    (print "a")
    (c (b x))))

(a #f)
