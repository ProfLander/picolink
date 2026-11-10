#lang picolink/r-lua/base

(require (only lua select)

         (only lua/ext/values foldl-values)

         (only r-lua/prelude/definition       define)
         (only r-lua/prelude/if               if)
         (only r-lua/prelude/interface        define-interface)
         (only r-lua/prelude/interface/equal  equal?)
         (only r-lua/prelude/s-lua            s-lua))

(provide (all-defined))

(define = equal?)

(define-interface Add
  [add __add
       (λ (a b)
         (s-lua (#%add a b)))])

(define (+ . [: vs #%values])
  (foldl-values add vs))

(define-interface Subtract
  [subtract __sub
            (λ (a b)
              (s-lua (#%sub a b)))])

(define-interface Negate
  [negate __unm
          (λ (a)
            (s-lua (#%neg a)))])

(define (- . [: vs #%values])
  (if (= (select "#" vs) 1)
      (negate vs)
      (foldl-values subtract vs)))

(define-interface Multiply
  [multiply __mul
            (λ (a b)
              (s-lua (#%mul a b)))])

(define (* . [: vs #%values])
  (foldl-values multiply vs))

(define-interface Divide
  [divide __div
          (λ (a b)
            (s-lua (#%div a b)))])

(define (/ . [: vs #%values])
  (foldl-values divide vs))

(define-interface Exponent
  [expt __pow
        (λ (a b)
          (s-lua (#%exp a b)))])

(define-interface Modulo
  [modulo __mod
          (λ (a b)
            (s-lua (#%mod a b)))])
