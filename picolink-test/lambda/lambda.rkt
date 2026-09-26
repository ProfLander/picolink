#lang picolink/lambda

(require (in lua print))

(define-syntax defun
  [(_ name arg body ...)
   #'(define name
       (λ (arg)
         body ...))])

(defun id x
  (print "id")
  x)

(define id2 (λ (x) x))

(((λ (x) x) (λ (y) y)) 1234)
