#lang racket/base

; Game Aligned Object Lisp, surface grammar

(require (for-syntax racket/base
                     syntax/parse)

         (only-in picolink2/gaol/ir
                  define-gaol-syntax
                  in-gaol
                  for-gaol)

         picolink2/gaol/passes/s-lua)

(provide (for-gaol let-values
                   define
                   lambda
                   (rename-out [lambda λ])))

(define-gaol-syntax define
  (syntax-parser
    [(_ ident:id e:expr)
     (in-gaol
      (syntax/loc this-syntax
        (#%define-values (ident) e)))]))

(define-gaol-syntax lambda
  (syntax-parser
    [(_ (arg ...) body ...)
     (in-gaol
      (syntax/loc this-syntax
        (#%lambda (arg ...) body ...)))]))

(define-gaol-syntax let-values
  (syntax-parser
    [(_ ([(arg ...) expr] ...) body ...)
     (in-gaol
      (syntax/loc this-syntax
        (#%let-values ([(arg ...) expr] ...)
                      body ...)))]))
