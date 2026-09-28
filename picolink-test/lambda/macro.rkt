#lang picolink/lambda

(require (in lua print))

(define-syntax defun
  [(_ name arg body ...)
   (define name
     (λ (arg)
       body ...))])

(defun id x
  (print "id")
  x)

(print (id 1234))
