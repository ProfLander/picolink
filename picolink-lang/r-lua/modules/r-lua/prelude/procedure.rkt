#lang picolink/r-lua/base

(require (only lua type)
         r-lua/prelude/definition
         r-lua/prelude/equality)

(provide (all-defined))

(define (procedure? v)
  (eq? (type v) "function"))
