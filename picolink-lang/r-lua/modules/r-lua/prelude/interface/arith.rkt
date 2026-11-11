#lang picolink/r-lua/base

(require (only r-lua/prelude/interface        define-interface)
         (only r-lua/prelude/s-lua            s-lua))

(provide (all-defined))

(define-interface Add
  [add %add
       (λ (a b)
         (s-lua (#%add a b)))])

(define-interface Subtract
  [subtract %subtract
            (λ (a b)
              (s-lua (#%sub a b)))])

(define-interface Negate
  [negate %negate
          (λ (a)
            (s-lua (#%neg a)))])

(define-interface Multiply
  [multiply %multiply
            (λ (a b)
              (s-lua (#%mul a b)))])

(define-interface Divide
  [divide %divide
          (λ (a b)
            (s-lua (#%div a b)))])

(define-interface Exponent
  [expt %expt
        (λ (a b)
          (s-lua (#%exp a b)))])

(define-interface Modulo
  [modulo %modulo
          (λ (a b)
            (s-lua (#%mod a b)))])
