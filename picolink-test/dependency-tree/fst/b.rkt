#lang picolink/input/lambda

(require (in snd/d d)
         (in snd/e e))

(provide b)

(define b (λ (x) (d (e x))))
