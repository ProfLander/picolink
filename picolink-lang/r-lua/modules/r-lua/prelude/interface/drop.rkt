#lang picolink/r-lua/base

(require (only r-lua/prelude/interface  define-interface))

(provide (all-defined))

(define-interface Drop
  [drop %drop])
