#lang racket/base

; module path utilities

(require racket/path)

(provide (all-defined-out))

(define (syntax-module-path stx)
  (string->symbol
   (path->string
    (path-replace-extension (find-relative-path
                             (current-directory)
                             (syntax-source stx))
                            ""))))
