#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-is-entry-point)

(define-values (current-is-entry-point)
  (make-parameter #t))
