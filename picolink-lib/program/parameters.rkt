#lang racket/base

(provide (all-defined-out))

(define current-program-scope
  (make-parameter #f))
