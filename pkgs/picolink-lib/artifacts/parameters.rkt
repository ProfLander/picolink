#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-compile-artifacts)

(define-values (current-compile-artifacts)
  (make-parameter #f))
