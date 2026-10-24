#lang picolink/r-lua/base

(require prelude/void)

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

(define-macro cond
  [(_ (~or (~optional [(~datum else) else-body ...+]
                      #:defaults ([[else-body 1] (list #'(void))]))
           [cond then-body ...+])
      ...)

   (for/foldr ([acc #'(begin else-body ...)])
              ([clause (in-list (syntax-e #'([cond then-body ...] ...)))])

     (syntax-parse clause
       [(cond body ...)
        #`(if cond
              (begin body ...)
              #,acc)]))])
