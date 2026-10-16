#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         
         racket/list

         syntax/parse

         picolink/syntax

         (only-in picolink/r-lua/anf
                  r-lua->r-lua-anf)

         (for-template racket/base
                       picolink/r-lua/ir
                       picolink/s-lua/ir))

(provide (all-defined-out))

(define (r-lua->s-lua stx)

  (let* ([anf   (r-lua->r-lua-anf stx)]
         [_ (println anf)]
         [s-lua (r-lua-anf->s-lua anf)]
         [_ (println s-lua)])
    s-lua))

(define (hoist-do stx)
  (syntax-parse stx
    #:literal-sets [s-lua-ir-literals]
    [(#%do ~! (#%block body ...))
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
  #:literal-sets [r-lua-ir-literals]

  [(#%begin ~! top ...)
   (let ([lctx this-syntax]
         [tops
          (filter-map
           (syntax-parser
             #:literal-sets [r-lua-ir-literals]
             [(#%define-macro ~! _ ...)
              #f]
             [top:top-level
              (attribute top.s-lua)])
           (syntax-e #'(top ...)))])

     (in-s-lua
      (syntax-parse tops
        #:literal-sets [s-lua-ir-literals]
        ; TODO: Reinstate tail-do elision in a way that is friendly
        ;       to backend s-lua splicing
        #;[[stmt ... (#%do ~! (#%block body ...))]
           (in-s-lua
            (syntax/loc lctx
              (#%chunk (#%block stmt ... body ...))))]
        [(top ...)
         (in-s-lua
          (syntax/loc lctx
            (#%chunk (#%block top ...))))])))])

(define-s-lua-syntax-class top-level
  #:literal-sets [r-lua-ir-literals]

  [(#%define ~! name val:expr)
   (in-s-lua
    (syntax/loc this-syntax
      (#%local [name] [val.s-lua])))]

  [(#%define-macro ~! _ ...)
   (unimplemented)]

  [e:expr
   (in-s-lua
    (syntax/loc this-syntax
      e.s-lua))])

(define-s-lua-syntax-class expr
  #:literal-sets [r-lua-ir-literals]

  [(#%set! ~! ident expr:expr)
   (in-s-lua
    (syntax/loc this-syntax
      (#%assign [ident] [expr.s-lua])))]

  [(#%let ~! ([name val:expr] ...)
          body:expr)
   (in-s-lua
    (with-syntax ([(body ...) (hoist-do #'body.s-lua)])
      (syntax/loc this-syntax
        (#%do
         (#%block
          (#%local [name ...] [val.s-lua ...])
          body ...)))))]

  [(#%letrec ~! ([name val:expr] ...)
             body:expr)
   (in-s-lua
    (with-syntax ([(body ...) (hoist-do #'body.s-lua)])
      (syntax/loc this-syntax
        (#%do
         (#%block
          (#%local [name ...])
          (#%assign [name ...] [val.s-lua ...])
          body ...)))))]

  [(#%declare ~! (name ...)
              body:expr)
   (in-s-lua
    (with-syntax ([(body ...) (hoist-do #'body.s-lua)])
      (syntax/loc this-syntax
        (#%do
         (#%block
          (#%local [name ...])
          body ...)))))]

  [(#%begin ~! body:expr ...)
   (in-s-lua
    (with-syntax ([(body ...)
                   (apply append (map hoist-do (attribute body.s-lua)))])
      (syntax/loc this-syntax
        (#%do
         (#%block body ...)))))]

  [(#%app ~! expr:expr ...+)
   (in-s-lua
    (syntax/loc this-syntax
      (#%call expr.s-lua ...)))]

  [(#%if ~! cond:expr then:expr else:expr)
   (with-syntax ([(then-body ...) (hoist-do #'then.s-lua)]
                 [(else-body ...) (hoist-do #'else.s-lua)])
     (in-s-lua
      (syntax/loc this-syntax
        (#%if cond
              (#%then (#%block then-body ...))
              (#%else (#%block else-body ...))))))]

  [(#%intrinsic ~! form)
   (in-s-lua
    (not-in-r-lua
     (syntax/loc this-syntax
       form)))]

  [v:val
   (in-s-lua
    (syntax/loc this-syntax
      v.s-lua))])

(define-s-lua-syntax-class val
  #:literal-sets [r-lua-ir-literals]

  [#%void
   (in-s-lua
    (syntax/loc this-syntax
      #%nil))]

  [(~and datum
         (~or _:boolean
              _:number
              _:string
              _:id))
   (in-s-lua
    (syntax/loc this-syntax
      datum))]

  [(#%abs ~! (arg ...) body:expr)
   (let ([body (hoist-do #'body.s-lua)])
     (in-s-lua
      (with-syntax ([(body ...) (drop-right body 1)]
                    [ret (last body)])
        (syntax/loc this-syntax
          '(#%function (arg ...)
                       (#%block
                        body ...
                        (#%return ret)))))))])

(define r-lua-anf->s-lua
  (syntax-parser
    [form:module-level
     (in-s-lua
      (syntax/loc this-syntax
        form.s-lua))]))
