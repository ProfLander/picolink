#lang picolink/input/lambda

(require (in gub
             sub))
(require (in gub
             nub))

(provide foo)
(provide foo bar baz)

(define foo
  (λ (x) x))

(foo x)
