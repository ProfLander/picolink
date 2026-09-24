#lang picolink/lambda

(require (in lua print))

(provide f)

(define f
  (λ (x)
    (print "f")
    x))
