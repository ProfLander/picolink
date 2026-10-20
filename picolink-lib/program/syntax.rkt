#lang racket/base

(provide (all-defined-out))

(define (program-ident lctx)
  (datum->syntax lctx '#%program))

(define (syntax-local-program lctx)
  (syntax-local-value (program-ident lctx)))
