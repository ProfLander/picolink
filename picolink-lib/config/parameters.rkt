#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-root
           current-config-path
           current-build-directory)

(define-values (current-root)
  (make-parameter #f))

(define-values (current-config-path)
  (make-parameter #f))

(define-values (current-build-directory)
  (make-parameter "build"))
