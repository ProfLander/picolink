#lang picolink/r-lua/base

(require (only lua type)
         prelude/equality)

(provide (all-defined))

(define boolean?
  (λ (v)
    (eq? (type v) "boolean")))
