#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide current-module-ident
           current-require-scope
           current-provide-scope
           current-foreign-scopes
           current-lexical-scopes)

(define-values (current-module-ident)
  (make-parameter #f))

(define-values (current-require-scope)
  (make-parameter #f))

(define-values (current-provide-scope)
  (make-parameter #f))

(define-values (current-foreign-scopes)
  (make-parameter null))

(define-values (current-lexical-scopes)
  (make-parameter null))
