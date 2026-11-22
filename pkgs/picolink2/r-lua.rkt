#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/syntax
                     syntax/parse)

         picolink2/s-lua)

(provide (for-syntax in-r-lua)
         (all-from-out racket/base)
         (for-space r-lua

                    let-values
                    define
                    λ

                    #%datum
                    #%define
                    #%lambda
                    #%app)

         r-lua

         (all-from-out picolink2/s-lua))

; utilities

(define-for-syntax in-r-lua
  (let ([intro (make-interned-syntax-introducer 'r-lua)])
    (λ (stx)
      (intro stx 'add))))

(define-syntax r-lua
  (syntax-parser
    [(_ body ...)
     (in-r-lua
      (syntax/loc this-syntax
        (begin
          body ...)))]))

(define-syntax define-r-lua-syntax
  (syntax-parser
    [(_ name transformer)
     (with-syntax ([name* (in-r-lua #'name)])
       (syntax/loc this-syntax
         (define-syntax name* transformer)))]))

; r-lua surface -> r-lua IR

(define-r-lua-syntax define
  (syntax-parser
    [(_ ident:id e:expr)
     (in-r-lua
      (syntax/loc this-syntax
        (#%define ident e)))]))

(define-r-lua-syntax λ
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

; r-lua IR -> s-lua IR

(define-for-syntax (local-expand-r-lua stx [ctx #f])
  (local-expand stx 'expression null ctx))

(define-r-lua-syntax #%datum
  (syntax-parser
    [(_ . d:expr)
     (in-s-lua
      (syntax/loc this-syntax
        (#%datum . d)))]))

(define-r-lua-syntax #%define
  (syntax-parser
    [(_ ident:id e:expr)
     (with-syntax* ([e (local-expand-r-lua #'e)])
       (in-s-lua
        (syntax/loc this-syntax
          (#%assign [ident] [e]))))]))

(define-r-lua-syntax #%lambda
  (syntax-parser
    [(_ (arg:id ...) body ...)

     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (attribute arg) #f ctx)

     (with-syntax ([(body ...)
                    (map (curryr local-expand-r-lua ctx)
                         (attribute body))])

       (in-s-lua
        (syntax/loc this-syntax
          (#%function (arg ...)
                      (#%block body ...)))))]))

(define-r-lua-syntax #%let-values
  (syntax-parser
    [(_ ([(arg:id ...) expr:expr] ...) body:expr ...)

     (define ctx (syntax-local-make-definition-context))
     (syntax-local-bind-syntaxes (apply append (attribute arg)) #f ctx)

     (with-syntax ([(body ...)
                    (map (curryr local-expand-r-lua ctx)
                         (attribute body))])

       (in-s-lua
        (syntax/loc this-syntax
          (#%do
           (#%block
            (#%call error (quote "unimplemented")))))))]))

(define-r-lua-syntax #%app
  (syntax-parser
    [(_ arg ...)
     (with-syntax ([(arg ...) (map local-expand-r-lua (attribute arg))])
       (in-s-lua
        (syntax/loc this-syntax
          (#%call arg ...))))]))
