#lang racket/base

(require picolink/scope
         picolink/module/parsed)

(provide (all-defined-out))

(define current-program-scope
  (make-parameter #f))

(define (lookup-in-program ident)
  (with-scope (current-program-scope)
    (lookup ident)))

(define (bind-in-program! ident stx)
  (with-scope (current-program-scope)
    (bind! ident stx)))
