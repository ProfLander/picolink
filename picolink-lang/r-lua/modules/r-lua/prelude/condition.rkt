#lang picolink/r-lua/base

(require (only r-lua/prelude/if          if)
         (only r-lua/prelude/sequencing  begin)
         (only r-lua/prelude/void        void))

(provide (all-defined))

(define-macro when
  [(_ cond body ...)
   (syntax/loc this-syntax
     (if cond
         (begin body ...)
         (void)))])

(define-macro unless
  [(_ cond body ...)
   (syntax/loc this-syntax
     (if cond
         (void)
         (begin body ...)))])

(define-macro cond
  [(_ (~or (~optional [(~datum else) else-body ...+]
                      #:defaults ([[else-body 1] (list #'(void))]))
           [cond then-body ...+])
      ...)

   (for/foldr ([acc (syntax/loc this-syntax
                      (begin else-body ...))])

              ([clause (in-list (syntax-e #'([cond then-body ...] ...)))])

     (syntax-parse clause
       [(cond body ...)
        (quasisyntax/loc this-syntax
          (if cond
              (begin body ...)
              #,acc))]))])
