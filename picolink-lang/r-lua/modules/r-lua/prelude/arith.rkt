#lang picolink/r-lua/base

(require (only r-lua/prelude/equality
               eq?)

         (only lua/ext/values
               foldl-values)

         r-lua/prelude/definition
         r-lua/prelude/interface
         r-lua/prelude/s-lua)

(provide (all-defined))

(define = eq?)

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
  [sub __sub
       (λ (a b)
         (s-lua (#%sub a b)))])

(define-interface Negate
  [neg __unm
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
                         (#%call neg #%vararg))))

                      (#%else
                       (#%block
                        (#%return
                         (#%call foldl-values sub #%vararg)))))))))

(define-interface Multiply
  [mul __mul
       (λ (a b)
         (s-lua (#%mul a b)))])

(define *
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values mul #%vararg))))))

(define-interface Divide
  [div __div
       (λ (a b)
         (s-lua (#%div a b)))])

(define /
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values div #%vararg))))))

(define-interface Power
  [expt __pow
        (λ (a b)
          (s-lua (#%exp a b)))])

(define-interface Modulo
  [modulo __mod
          (λ (a b)
            (s-lua (#%mod a b)))])
