#lang racket/base

; Game Aligned Object Lisp, surface grammar

(require (for-syntax racket/base
                     syntax/parse)

         (only-in picolink2/gaol/ir
                  define-gaol-forms
                  for-gaol)

         picolink2/gaol/passes/s-lua)

(provide (for-gaol begin
                   define-values
                   let-values
                   letrec-values
                   lambda
                   (rename-out [lambda λ])
                   if
                   set!

                   define
                   let
                   letrec))

; Core forms

(define-gaol-forms

  [(begin e:expr ...)
   (syntax/loc this-syntax
     (#%begin e ...))]

  [(define-values (ident:id ...) e:expr)
   (syntax/loc this-syntax
     (#%define-values (ident ...) e))]

  [(let-values ([(arg ...) expr] ...) body ...)
   (syntax/loc this-syntax
     (#%let-values ([(arg ...) expr] ...)
                   body ...))]

  [(letrec-values ([(arg ...) expr] ...) body ...)
   (syntax/loc this-syntax
     (#%letrec-values ([(arg ...) expr] ...)
                      body ...))]

  [(lambda (arg ...) body ...)
   (syntax/loc this-syntax
     (#%lambda (arg ...) body ...))]

  [(if cond then else)
   (syntax/loc this-syntax
     (#%if cond then else))]

  [(set! target expr)
   (syntax/loc this-syntax
     (#%set! target expr))])

; Extensions

(define-gaol-forms

  [(define ident:id e:expr)
   (syntax/loc this-syntax
     (define-values (ident) e))]

  #;[(define (ident:id arg:id ...) body:expr ...)
   (syntax/loc this-syntax
     (define ident
       (lambda (arg ...)
         body ...)))]

  [(let ([ident:id bind:expr] ...)
     body:expr ...)

   (syntax/loc this-syntax
     (let-values ([(ident) bind] ...)
       body ...))]

  [(letrec ([ident:id bind:expr] ...)
     body:expr ...)

   (syntax/loc this-syntax
     (letrec-values ([(ident) bind] ...)
       body ...))])
