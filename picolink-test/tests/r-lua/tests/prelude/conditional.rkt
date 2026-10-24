#lang picolink/r-lua

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
