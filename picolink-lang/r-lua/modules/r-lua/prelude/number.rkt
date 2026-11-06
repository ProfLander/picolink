#lang picolink/r-lua/base

(require (only lua type)
         r-lua/prelude/definition
         r-lua/prelude/equality)

(provide (all-defined))

(define (number? v)
  (eq? (type v) "number"))
