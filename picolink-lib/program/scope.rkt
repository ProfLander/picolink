#lang racket/base

(require picolink/ee-lib
         picolink/module/parsed)

(provide (all-defined-out))

(define current-program-scope
  (make-parameter #f))

(define (lookup-in-program ident)
  (lookup-in (current-program-scope) ident parsed-module?))

(define (bind-in-program! ident stx)
  (bind-in! (current-program-scope) ident stx))
