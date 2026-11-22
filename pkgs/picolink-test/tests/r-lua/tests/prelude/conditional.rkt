#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/condition   cond)
         (only r-lua/prelude/lambda      λ)

         r-lua/prelude/boolean
         r-lua/prelude/number
         r-lua/prelude/string
         r-lua/prelude/procedure)

(define print-type
  (λ (v)
    (cond
      [(boolean? v)    (print "is bool")]
      [(number? v)     (print "is number")]
      [(string? v)     (print "is string")]
      [(procedure? v)  (print "is procedure")])))

(print-type #f)
(print-type #t)
(print-type 1234)
(print-type "hello, world")
(print-type (λ (x) x))
