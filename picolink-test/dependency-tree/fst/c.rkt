#lang picolink/input/lambda

(require (in snd/e e)
         (in snd/f f))

(provide c)

(define c (λ (x) (e (f x))))
