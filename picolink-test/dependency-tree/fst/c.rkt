#lang picolink/lambda

(require (in lua print)
         (in snd/e e)
         (in snd/f f))

(provide c)

(define c
  (λ (x)
    (print "c")
    (f (e x))))
