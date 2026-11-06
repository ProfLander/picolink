#lang picolink/r-lua/base

(require r-lua/prelude/arith
         r-lua/prelude/lambda
         r-lua/prelude/sequencing
         r-lua/prelude/void)

(provide (all-defined))

(define-macro for
  [(_ ([i ((~datum in-range) from to)])
      body ...)

   (syntax/loc this-syntax
     (letrec ([body* (λ (i)
                       body ...
                       (if (< i to)
                           (body* (+ i 1))
                           (void)))])
       (body* from)))])

(define-macro for/fold
  [(_ ([acc-id acc])
      ([next-id ((~datum in-range) from to)])
      body ...)

   (syntax/loc this-syntax
     (letrec ([body* (λ (acc-id next-id)
                       (let ([acc-id (begin body ...)])
                         (if (= next-id to)
                             acc-id
                             (body* acc-id (+ next-id 1)))))])
       (body* acc from)))])
