#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-is-entry-point
           current-backend)

(define-values (current-is-entry-point)
  (make-parameter #t))

(define-values (current-backend)
  (make-parameter 'picolink/racket))
