#lang racket/base

(require racket/dict
         picolink/syntax)

(provide (all-defined-out))

(struct/cross-phase alist [box])

(define (make-alist)
  (alist (box null)))

(define (alist-ref self key)
  (let ([binding (assoc key (unbox (alist-box self)) datum=?)])
    (and binding (cdr binding))))

(define (alist-set! self key val)
  (set-box! (alist-box self)
            (cons (cons key val)
                  (unbox (alist-box self)))))

(define (alist-keys self)
  (dict-keys (unbox (alist-box self))))

(define (alist-empty? self)
  (dict-empty? (unbox (alist-box self))))

(define (in-alist self)
  (in-dict (unbox (alist-box self))))
