#lang picolink/r-lua

(define cons
  (λ (a b)
    (#%intrinsic (#%table a b))))

(let ([a 1]
      [b 2]
      [c 3])

  (cons a (cons b c)))
