#lang picolink/r-lua

(require (only-in lua
                  print)

         (only-in prelude/function
                  identity))

(let ([a identity]
      [b (λ (x) (λ (x) x))]
      [c (λ (x) (x))]
      [d (λ (x) (λ () x))])
  ((a (b 8)) (c (d 7))))
