#lang racket/base

(provide local-expand-s-lua
         in-s-lua)

(define (local-expand-s-lua stx [ctx #f])
  (local-expand stx 'expression null ctx))

(define in-s-lua
  (let ([intro (make-interned-syntax-introducer 's-lua)])
    (λ (stx)
      (intro stx 'add))))
