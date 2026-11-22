#lang racket/base

(require compiler/cm)

(parameterize ([error-print-width 1000000]
               [print-syntax-width +inf.0]
               [print-graph #t])
  (managed-compile-zo
   "/home/lander/src/picolink/picolink-test/tests/r-lua/tests/let/basic.rkt"))
