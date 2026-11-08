#lang picolink/r-lua/base

(require r-lua/prelude/interface)

(provide (all-defined))

(define-interface Equal
  [eq? __eq
       (λ (a b)
         (s-lua (#%eq a b)))])
