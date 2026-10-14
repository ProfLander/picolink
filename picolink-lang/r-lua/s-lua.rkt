#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         
         racket/list

         syntax/parse

         picolink/syntax

         (only-in picolink/r-lua/anf
                  r-lua->r-lua-anf)

         (for-template racket/base
                       picolink/s-lua/ir))

(provide (all-defined-out))

(define (r-lua->s-lua stx)

  (let* ([anf   (r-lua->r-lua-anf stx)]
         [s-lua (r-lua-anf->s-lua anf)])
    s-lua))

(define (hoist-do stx)
  (syntax-parse stx
    #:datum-literals [#%do #%block]
    [(#%do (#%block body ...))
     (apply append (map hoist-do (attribute body)))]
    [_ (list this-syntax)]))

(define (unimplemented)
  (error "unimplemented"))

(define-syntax define-s-lua-syntax-class
  (syntax-parser
    [(_ name body ...)
     #'(define-parser-like-syntax-class name s-lua
         body ...)]))

(define-s-lua-syntax-class module-level
  #:datum-literals [#%begin]

  [(#%begin top ...)
   (let ([lctx this-syntax]
         [tops
          (filter-map
           (syntax-parser
             #:datum-literals [#%define-syntax]
             [(#%define-syntax _ ...)
              #f]
             [top:top-level
              (attribute top.s-lua)])
           (syntax-e #'(top ...)))])

     (syntax-parse tops
       #:datum-literals [#%do #%block]
       ; TODO: Reinstate tail-do elision in a way that is friendly
       ;       to backend s-lua splicing
       #;[[stmt ... (#%do (#%block body ...))]
          (syntax/loc lctx
            (#%chunk (#%block stmt ... body ...)))]
       [(top ...)
        (syntax/loc lctx
          (#%chunk (#%block top ...)))]))])

(define-s-lua-syntax-class top-level
  #:datum-literals [#%define
                    #%define-syntax
                    #%app]

  [(#%define name val:expr)
   (syntax/loc this-syntax
     (#%local [name] [val.s-lua]))]

  [(#%define-syntax _ ...)
   (unimplemented)]

  [e:expr
   (syntax/loc this-syntax
     e.s-lua)])

(define-s-lua-syntax-class expr
  #:datum-literals [#%let
                    #%begin
                    #%set!
                    #%app
                    #%if]

  [(#%set! ident expr:expr)
   (syntax/loc this-syntax
     (#%assign [ident] [expr.s-lua]))]

  [(#%let ([name val:expr] ...)
          body:expr)
   (with-syntax ([(body ...) (hoist-do #'body.s-lua)])
     (syntax/loc this-syntax
       (#%do
        (#%block
         (#%local [name ...] [val.s-lua ...])
         body ...))))]

  [(#%begin body:expr ...)
   (with-syntax ([(body ...)
                  (apply append (map hoist-do (attribute body.s-lua)))])
     (syntax/loc this-syntax
       (#%do
        (#%block body ...))))]

  [(#%app expr:expr ...+)
   (syntax/loc this-syntax
     (#%call expr.s-lua ...))]

  [(#%if cond:expr then:expr else:expr)
   (syntax/loc this-syntax
     (#%if cond
           (#%then then)
           (#%else else)))]

  [v:val
   (syntax/loc this-syntax
     v.s-lua)])

(define-s-lua-syntax-class val
  #:datum-literals [#%void
                    #%abs]

  [#%void (syntax/loc this-syntax #%nil)]
  [b:boolean (syntax/loc this-syntax b)]
  [n:number (syntax/loc this-syntax n)]
  [s:string (syntax/loc this-syntax s)]
  [v:id (syntax/loc this-syntax v)]

  [(#%abs (arg ...)
          body:expr)
   (let ([body (hoist-do #'body.s-lua)])
     (with-syntax ([(body ...) (drop-right body 1)]
                   [ret (last body)])
       (syntax/loc this-syntax
         '(#%function (arg ...)
                      (#%block
                       body ...
                       (#%return ret))))))])

(define r-lua-anf->s-lua
  (syntax-parser
    [form:module-level
     (syntax/loc this-syntax
       form.s-lua)]))
