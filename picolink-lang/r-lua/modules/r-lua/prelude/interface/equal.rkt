#lang picolink/r-lua/base

(require (only lua/ext/ops  ==)

         (only r-lua/prelude/interface  define-interface))

(provide (all-defined))

(define-interface Equal
  [equal? %equal? ==])
