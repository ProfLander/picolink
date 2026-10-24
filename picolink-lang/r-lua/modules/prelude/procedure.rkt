#lang picolink/r-lua/base

(require (only lua type)
         prelude/equality)

(provide (all-defined))

(define procedure?
  (λ (v)
    (eq? (type v) "function")))
