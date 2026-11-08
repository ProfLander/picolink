#lang picolink/r-lua/base

(require (rename lua print lua-print)

         (only r-lua/prelude/condition   cond)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/equality    equal?)
         (only r-lua/prelude/void        void void?))

(provide (all-defined))

(define (print v)
  (cond
    [(equal? v #f) (void (lua-print "#f"))]
    [(equal? v #t) (void (lua-print "#t"))]
    [(void? v)     (void)]
    [else          (void (lua-print v))]))
