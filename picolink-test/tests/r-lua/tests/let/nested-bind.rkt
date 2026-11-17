#lang picolink/r-lua/base

(require (only r-lua/runtime/print     print)

         (only r-lua/prelude/binding   let)
         (only r-lua/prelude/function  identity)

         r-lua/prelude/number)

(let ([x 4]
      [y (let ([x 3])
           (identity x))])
  (print y)
  x)
