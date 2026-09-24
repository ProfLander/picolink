#lang picolink/lambda

(require (in lua print))

(provide d)

(define d
  (λ (x)
    (print "d")
    x))
