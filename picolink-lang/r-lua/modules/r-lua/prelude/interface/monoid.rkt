#lang picolink/r-lua/base

(require (only lua/ext/values
               foldl-values)

         r-lua/prelude/definition
         r-lua/prelude/interface
         r-lua/prelude/s-lua)

(provide (all-defined))

(define-interface Monoid
  [mappend __concat
           (λ (a b)
             (s-lua (#%cat a b)))])

(define <>
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values mappend #%vararg))))))
