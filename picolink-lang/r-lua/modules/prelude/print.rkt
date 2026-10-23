#lang picolink/r-lua/base

(require (rename lua print %print)
         prelude/conditional
         prelude/equality
         prelude/void)

(provide (all-defined))

(define print
  (λ (v)
    (unless (eq? v %void)
      (%print v))
    %void))
