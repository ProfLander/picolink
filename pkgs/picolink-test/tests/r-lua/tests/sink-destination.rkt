#lang picolink/r-lua/base

(require (only r-lua/prelude/binding    let letrec)
         (only r-lua/prelude/definition define)
         (only r-lua/prelude/if         if)
         (only r-lua/prelude/lambda     λ)

         r-lua/prelude/string)

(if #f

    (let ([v (if #f "yes" "no")])
      v)

    (letrec ([cond  #f]
             [check (λ () (if cond "ja" "nein"))])
      (check)))

(let ([method "foo"])
  (let ([method (if method
                    method
                    "bar")])
    (if method
        method
        "baz")))
