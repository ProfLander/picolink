#lang picolink/r-lua/base

(require (only lua type)
         prelude/equality
         prelude/surface)

(provide (all-defined))

(define (boolean? v)
  (eq? (type v) "boolean"))
