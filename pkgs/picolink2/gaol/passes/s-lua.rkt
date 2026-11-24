#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/syntax
                     syntax/parse)

         (only-in picolink2/gaol/ir
                  define-gaol-syntax
                  in-gaol
                  local-expand-gaol
                  for-gaol)

         (only-in picolink2/s-lua/ir
                  in-s-lua)

         picolink2/s-lua/passes/lua)

(provide (all-from-out racket/base)

         (for-gaol #%datum
                   #%define-values
                   #%lambda
                   #%app)

         (all-from-out picolink2/s-lua/passes/lua))

; gaol IR -> s-lua IR

(define-gaol-syntax #%datum
  (syntax-parser
    [(_ . d:expr)
     (in-s-lua
      (syntax/loc this-syntax
        (#%datum . d)))]))

(define-gaol-syntax #%define-values
  (syntax-parser
    [(_ (ident:id ...) e:expr)
     (with-syntax* ([e (local-expand-gaol #'e)])
       (in-s-lua
        (syntax/loc this-syntax
          (#%assign [ident ...] [e]))))]))

(define-gaol-syntax #%let-values
  (syntax-parser
    [(_ ([(arg:id ...) expr:expr] ...) body:expr ...)

     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (apply append (attribute arg)) #f ctx)

     (with-syntax ([(body ...)
                    (map (curryr local-expand-gaol ctx)
                         (attribute body))])

       (in-s-lua
        (syntax/loc this-syntax
          (#%do
           (#%block
            (#%call error (quote "unimplemented")))))))]))

(define-gaol-syntax #%lambda
  (syntax-parser
    [(_ (arg:id ...) body ...)

     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (attribute arg) #f ctx)

     (with-syntax ([(body ... ret)
                    (map (curryr local-expand-gaol ctx)
                         (attribute body))])

       (in-s-lua
        (syntax/loc this-syntax
          (#%function (arg ...)
                      (#%block body ... (#%return ret))))))]))

(define-gaol-syntax #%app
  (syntax-parser
    [(_ arg ...)
     (with-syntax ([(arg ...) (map local-expand-gaol (attribute arg))])
       (in-s-lua
        (syntax/loc this-syntax
          (#%call arg ...))))]))
