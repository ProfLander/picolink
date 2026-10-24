#lang picolink/r-lua/base

(require (only lua type)
         prelude/equality)

(provide (all-defined))

(define number?
  (λ (v)
    (eq? (type v) "number")))
