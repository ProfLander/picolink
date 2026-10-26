#lang picolink/r-lua/base

(require (rename lua print %print)
         prelude/conditional
         prelude/equality
         prelude/surface
         prelude/void)

(provide (all-defined))

(define (print v)
  (cond
    [(eq? v #f) (void (%print "#f"))]
    [(eq? v #t) (void (%print "#t"))]
    [(void? v)  (void)]
    [else       (void (%print v))]))
