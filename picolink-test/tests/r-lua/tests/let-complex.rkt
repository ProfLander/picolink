#lang picolink/r-lua

(require (in lua print)
         (in function identity))

(let ([a identity]
      [b (λ (x) (λ (x) x))]
      [c (λ (x) (x))]
      [d (λ (x) (λ () x))])
  (print ((a (b 8)) (c (d 7)))))
