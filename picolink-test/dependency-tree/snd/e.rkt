#lang picolink/lambda

(require (in lua print))

(provide e)

(define e
  (λ (x)
    (print "e")
    x))
