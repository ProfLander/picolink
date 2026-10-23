#lang picolink/r-lua

(let ([a identity]
      [b (λ (x) (λ (x) x))]
      [c (λ (x) (x))]
      [d (λ (x) (λ () x))])
  ((a (b 6)) (c (d 5))))
