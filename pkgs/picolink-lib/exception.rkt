#lang racket/base

(require racket/contract)

(provide (all-defined-out))

(define/contract (raise-unbound-variable-error stx)
  (-> syntax? none/c)
  (raise
   (make-exn:fail:user
    (format "~a: unbound variable\n in ~a:~a:~a"
            (syntax-e stx)
            (syntax-source stx)
            (syntax-line stx)
            (syntax-column stx))
    (continuation-marks #f))))
