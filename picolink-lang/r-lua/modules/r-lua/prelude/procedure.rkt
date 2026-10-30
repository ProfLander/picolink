#lang picolink/r-lua/base

(require (only lua type)
         r-lua/prelude/equality
         r-lua/prelude/surface)

(provide (all-defined))

(define (procedure? v)
  (eq? (type v) "function"))
