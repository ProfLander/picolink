#lang picolink/r-lua/base

(require (only lua/ext/values foldl-values)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/equality    equal?)
         (only r-lua/prelude/interface   define-interface)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define = equal?)

(define-interface Add
  [add __add
       (λ (a b)
         (s-lua (#%add a b)))])

(define +
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values add #%vararg))))))

(define-interface Subtract
  [subtract __sub
            (λ (a b)
              (s-lua (#%sub a b)))])

(define-interface Negate
  [negate __unm
          (λ (a)
            (s-lua (#%neg a)))])

(define -
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%if (#%eq (#%call select "#" #%vararg) 1)

                      (#%then
                       (#%block
                        (#%return
                         (#%call negate #%vararg))))

                      (#%else
                       (#%block
                        (#%return
                         (#%call foldl-values subtract #%vararg)))))))))

(define-interface Multiply
  [multiply __mul
            (λ (a b)
              (s-lua (#%mul a b)))])

(define *
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values multiply #%vararg))))))

(define-interface Divide
  [divide __div
          (λ (a b)
            (s-lua (#%div a b)))])

(define /
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values divide #%vararg))))))

(define-interface Exponent
  [expt __pow
        (λ (a b)
          (s-lua (#%exp a b)))])

(define-interface Modulo
  [modulo __mod
          (λ (a b)
            (s-lua (#%mod a b)))])
