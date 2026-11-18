#lang picolink/r-lua/base

(require (only r-lua/runtime/print     print)

         (only r-lua/prelude/binding   let let-values)
         (only r-lua/prelude/function  identity)
         (only r-lua/prelude/if        if)
         (only r-lua/prelude/values    values)

         r-lua/prelude/number
         r-lua/prelude/string)

(if #t 1234 5678)

(if #f 1234 5678)

(identity
 (if #f
     "a"
     "b"))

(if (let ([x #t]
          [y #f])
      y)
    "one"
    "two")

(let-values ([(a b) (if #t (values 1 2) (values 3 4))])
  (print "values:")
  (print a)
  (print b))
