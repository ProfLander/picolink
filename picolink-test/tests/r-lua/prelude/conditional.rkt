#lang picolink/r-lua

(provide (all-defined))

(define-macro when
  [(_ cond body ...)
   #'(if cond
         (begin body ...)
         (void))])

(define-macro unless
  [(_ cond body ...)
   #'(if cond
         (void)
         (begin body ...))])
