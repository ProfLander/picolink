#lang picolink/r-lua/base

(require (only lua type)
         r-lua/prelude/equality
         r-lua/prelude/surface)

(provide (all-defined))

(define (number? v)
  (eq? (type v) "number"))
