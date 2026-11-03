#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-root
           current-backend
           current-build-directory
           current-parallel
           current-config-directory)

(define-values (current-root)
  (make-parameter #f))

(define-values (current-backend)
  (make-parameter #f))

(define-values (current-config-directory)
  (make-parameter #f))

(define-values (current-build-directory)
  (make-parameter "build"))

(define-values (current-parallel)
  (make-parameter #t))
