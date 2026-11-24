#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/syntax
                     syntax/parse)

         (only-in picolink2/gaol/ir
                  define-gaol-pass-form
                  define-gaol-pass
                  local-expand-gaol
                  for-gaol)

         picolink2/s-lua/passes/lua)

(provide (all-from-out racket/base)

         (for-gaol #%datum
                   #%define-values
                   #%let-values
                   #%lambda
                   #%app)

         (all-from-out picolink2/s-lua/passes/lua))

; gaol IR -> s-lua IR

(define-gaol-pass s-lua

  [(#%datum . d:expr)
   (syntax/loc this-syntax
     (#%datum . d))]

  [(#%define-values (ident:id ...) e:expr)
   #:with e* (local-expand-gaol #'e)
   (syntax/loc this-syntax
     (#%assign [ident ...] [e*]))]

  [(#%let-values ([(arg:id ...) expr:expr] ...)
                 body:expr ...)

   #:do [(define ctx (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes (apply append (attribute arg))
                                     #f
                                     ctx)]

   #:with (body* ...) (map (curryr local-expand-gaol ctx)
                           (attribute body))

   (syntax/loc this-syntax
     (#%do
      (#%block
       (#%call error (quote "unimplemented")))))]

  [(#%lambda (arg:id ...)
             body ...)

   #:do [(define ctx (syntax-local-make-definition-context))
         (syntax-local-bind-syntaxes (attribute arg) #f ctx)]

   #:with (body* ... ret) (map (curryr local-expand-gaol ctx)
                               (attribute body))

   (syntax/loc this-syntax
     (#%function (arg ...)
                 (#%block body* ... (#%return ret))))]

  [(#%app arg ...)
   #:with (arg* ...) (map local-expand-gaol (attribute arg))
   (syntax/loc this-syntax
     (#%call arg* ...))])
