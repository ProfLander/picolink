#lang picolink/r-lua/base

(require (only lua newproxy))

(require prelude/equality
         prelude/surface)

(provide (all-defined))

(define %void
  (newproxy))

(define (void? v)
  (eq? v %void))

(define-macro void
  [(_ body ...)
   #'(begin
       body ...
       %void)])
