#lang picolink/r-lua/base

(require (only r-lua/prelude/definition define)
         (only r-lua/prelude/binding    let)

         r-lua/prelude/number)

(define let-bound
  (let ([x 1]
        [y 2])
    y))
