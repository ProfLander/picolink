#lang picolink/r-lua/base

(require (only lua type)
         r-lua/prelude/definition
         r-lua/prelude/equality)

(provide (all-defined))

(define (boolean? v)
  (eq? (type v) "boolean"))
