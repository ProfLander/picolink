#lang picolink/r-lua/base

(require (rename lua error lua-error)
         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (error msg)
  (lua-error msg))
