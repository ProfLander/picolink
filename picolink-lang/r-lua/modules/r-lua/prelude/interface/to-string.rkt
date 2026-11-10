#lang picolink/r-lua/base

(require (only lua  tostring)

         (only r-lua/prelude/interface  define-interface))

(provide (all-defined))

(define-interface ToString
  [to-string __tostring
             (λ (a)
               (tostring a))])
