#lang picolink/r-lua/base

(require (only r-lua/prelude/binding   let)
         (only r-lua/prelude/function  thunk identity)
         (only r-lua/prelude/lambda    λ)

         r-lua/prelude/number)

(let ([a identity]
      [b (λ (x) (λ (x) x))]
      [c (λ (x) (x))]
      [d (λ (x) (thunk x))])
  ((a (b 6)) (c (d 5))))
