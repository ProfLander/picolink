#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-backend)

(define-values (current-backend)
  (make-parameter #f))
