#lang picolink/r-lua/base

(require (rename lua print %print)
         r-lua/prelude/conditional
         r-lua/prelude/equality
         r-lua/prelude/surface
         r-lua/prelude/void)

(provide (all-defined))

(define (print v)
  (cond
    [(eq? v #f) (void (%print "#f"))]
    [(eq? v #t) (void (%print "#t"))]
    [(void? v)  (void)]
    [else       (void (%print v))]))
