#lang picolink/lambda

(define-syntax defun
  [(_ name arg body)
   #'(define name
       (λ (arg) body))])

(defun id x x)

(define id2 (λ (x) x))

(((λ (x) x) (λ (y) y)) 1234)
