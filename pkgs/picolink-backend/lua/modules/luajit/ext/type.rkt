#lang picolink/r-lua/base

(require (only lua type)

         (only lua/ext/ops ==)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (cdata? v)
  (== (type v) "cdata"))
