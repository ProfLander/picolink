#lang picolink/r-lua/base

(require (only lua newproxy))

(require prelude/equality)

(provide (all-defined))

(define %void
  (newproxy))

(define void?
  (λ (v)
    (eq? v %void)))

(define-macro void
  [(_ body ...)
   #'(begin
       body ...
       %void)])
