#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-program-scope)

(define-values (current-program-scope)
  (make-parameter #f))
