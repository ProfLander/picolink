#lang picolink/r-lua/base

(require (rename lua print lua-print)

         (only r-lua/runtime/void                 void void?)

         (only r-lua/prelude/condition            cond)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/interface/equal      equal?)
         (only r-lua/prelude/interface/to-string  to-string))

(provide print)

(define (%print . [: vs #%values])
  (void (lua-print vs)))

(define (print v)
  (cond
    [(equal? v #f) (%print "#f")]
    [(equal? v #t) (%print "#t")]
    [(void? v)     v]
    [else          (%print v)]))
