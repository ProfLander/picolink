#lang racket/base

(require racket/syntax)

(provide (all-defined-out))

(define (module-ident lctx [mod-ident lctx])
  (format-id lctx "~a:#%module" mod-ident))

(define (syntax-local-module lctx [mod-ident lctx])
  (syntax-local-eval (module-ident lctx mod-ident)))
