#lang picolink/r-lua

(provide void)

(define %void
  "#<void>")

(define-macro void
  [(_ body ...)
   #'(begin
       body ...
       %void)])
