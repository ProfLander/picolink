#lang racket/base

(require racket/contract
         (for-template racket/base))

(provide (all-defined-out))

(define/contract (make-input-module lctx input)
  (-> syntax? syntax? syntax?)

  (with-syntax ([input-id (syntax-local-identifier-as-binding
                           (datum->syntax lctx 'input))]
                [input input])

    #'(#%plain-module-begin
       (provide input-id)
       (define input-id input))))
