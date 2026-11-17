#lang picolink/r-lua/base

(require (only r-lua/prelude/binding   let)
         (only r-lua/prelude/function  identity)

         r-lua/prelude/number)

(let ([x 2])
    x
    (let ([y (identity x)])
      y))
