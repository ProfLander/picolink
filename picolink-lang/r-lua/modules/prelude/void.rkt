#lang picolink/r-lua/base

(provide %void
         void)

(define %void
  "#<void>")

(define-macro void
  [(_ body ...)
   #'(begin
       body ...
       %void)])
