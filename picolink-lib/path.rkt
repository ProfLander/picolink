#lang racket/base

(provide (all-defined-out))

(define (module-path->file-path path)
  (path-replace-extension path ".rkt"))
