#lang picolink/lambda

(require (in snd/d d)
         (in snd/e e))

(provide b)

(define b
  (λ (x)
    (print "b")
    (e (d x))))
