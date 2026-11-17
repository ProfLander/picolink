#lang picolink/r-lua/base

(require (only r-lua/runtime/print       print)

         (only r-lua/prelude/binding     let-values)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/function    identity)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/list        list)
         (only r-lua/prelude/vector      vector)

         r-lua/prelude/number
         r-lua/prelude/string)

(identity 1)

(define vararg-list (λ [: x list]
                      x))

(vararg-list 2 4 6)

(define vararg-vector (λ (a . [: x vector])
                        x))

(vararg-vector "foo" 3 2 1)

(define vararg-values (λ (a . [: x #%values])
                        x))

(let-values ([(a b c) (vararg-values "foo" 9 7 5)])
  (print a)
  (print b)
  (print c))
