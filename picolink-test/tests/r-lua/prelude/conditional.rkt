#lang picolink/r-lua

(provide when
         unless)

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
