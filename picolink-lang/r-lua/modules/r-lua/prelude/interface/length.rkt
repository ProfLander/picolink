#lang picolink/r-lua/base

(require (only r-lua/prelude/interface  define-interface)
         (only r-lua/prelude/s-lua      s-lua))

(provide (all-defined))

(define-interface Length
  [length %length
          (λ (a)
            (s-lua (#%length a)))])
