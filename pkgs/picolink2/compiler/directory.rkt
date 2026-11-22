#lang racket/base

(provide (all-defined-out))

(define current-build-directory
  (make-parameter #f))
