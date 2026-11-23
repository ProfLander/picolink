#lang racket/base

; Game Aligned Object Lisp, surface grammar

(require (for-syntax racket/base
                     syntax/parse)

         picolink2/gaol/passes/s-lua)

(provide (for-space r-lua 
                    let-values
                    define
                    lambda
                    (rename-out [lambda λ])))

(define-r-lua-syntax define
  (syntax-parser
    [(_ ident:id e:expr)
     (in-r-lua
      (syntax/loc this-syntax
        (#%define-values (ident) e)))]))

(define-r-lua-syntax lambda
  (syntax-parser
    [(_ (arg ...) body ...)
     (in-r-lua
      (syntax/loc this-syntax
        (#%lambda (arg ...) body ...)))]))

(define-r-lua-syntax let-values
  (syntax-parser
    [(_ ([(arg ...) expr] ...) body ...)
     (in-r-lua
      (syntax/loc this-syntax
        (#%let-values ([(arg ...) expr] ...)
                      body ...)))]))
