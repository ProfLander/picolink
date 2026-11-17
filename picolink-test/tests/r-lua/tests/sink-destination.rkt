#lang picolink/r-lua/base

(require (only r-lua/runtime/void)

         (only r-lua/prelude/binding    let letrec)
         (only r-lua/prelude/definition define)
         (only r-lua/prelude/if         if)
         (only r-lua/prelude/lambda     λ)

         r-lua/prelude/number)

(if #f

    (let ([v (if #f 1 2)])
      v)

    (letrec ([cond  #f]
             [check (λ () (if cond 1 2))])
      (check)))
