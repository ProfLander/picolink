#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define (.. a b)
  (s-lua (#%cat a b)))
