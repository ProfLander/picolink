#lang racket/base

(require racket/generic)

(provide (all-defined-out))

(define-generics output
  (source output)
  (compile output))
