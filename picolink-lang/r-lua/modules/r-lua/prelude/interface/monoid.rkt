#lang picolink/r-lua/base

(require (only lua/ext/values  foldl-values)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/interface   define-interface)
         (only r-lua/prelude/s-lua       s-lua))

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
