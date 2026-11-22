#lang picolink/r-lua/base

(require (only r-lua/prelude/arith    +)
         (only r-lua/prelude/binding  let-values)
         (only r-lua/prelude/values   values)

         r-lua/prelude/number)

(let-values ([(a b c) (values 1 2 3)])
  (+ a b c))
