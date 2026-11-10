#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define (send t k . [: vs values])
  ((s-lua (#%member t 'k)) t vs))
