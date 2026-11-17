#lang picolink/r-lua/base

(require (only r-lua/prelude/binding  let)

         r-lua/prelude/number)

(let ([x 1]
      [y 2])
  y
  x)
