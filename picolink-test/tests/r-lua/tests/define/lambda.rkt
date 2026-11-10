#lang picolink/r-lua

(define identity (λ (x) x))

(identity 1)

(define vararg-list (λ [: x list]
                      x))

(vararg-list 2 4 6)

(define vararg-vector (λ (a . [: x vector])
                        x))

(vararg-vector "foo" 3 2 1)

(define vararg-values (λ (a . [: x values])
                        x))

(let-values ([(a b c) (vararg-values "foo" 9 7 5)])
  (print a)
  (print b)
  (print c))
