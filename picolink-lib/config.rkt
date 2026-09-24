#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define current-backend
  (make-parameter 'lua))

(define/contract (set-current-backend output)
  (-> string? void)
  (current-backend (string->symbol output)))

(define current-entry-point
  (make-parameter (string->path "../picolink-test/intrinsics/string")))
