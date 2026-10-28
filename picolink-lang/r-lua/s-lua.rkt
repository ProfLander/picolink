#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         
         racket/list
         racket/syntax

         syntax/parse

         picolink/syntax

         (only-in picolink/r-lua/anf
                  r-lua->r-lua-anf)

         (for-template racket/base
                       picolink/r-lua/ir
                       picolink/s-lua/ir))

(provide (all-defined-out))

(define (current-debug) #f)

(define (r-lua->s-lua stx)

  (let ([debug (current-debug)])

    (define anf (r-lua->r-lua-anf stx))

    (when debug
      (displayln "anf:")
      (println anf))

    (define s-lua (r-lua-anf->s-lua anf))

    (when debug
      (displayln "s-lua:")
      (println s-lua))

    s-lua))

(define (hoist-do stx)

  (when (current-debug)
    (displayln "hoist-do:")
    (println stx))

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


; TODO: Reinstate tail-do elision in a way that is friendly
;       to backend s-lua splicing
(define-s-lua-syntax-class module-level
  #:literal-sets [r-lua-ir-literals]

  [(#%begin ~! top:top-level ...)
   (in-s-lua
    (with-syntax ([([top ...] ...) (attribute top.s-lua)])
      (syntax/loc this-syntax
       (#%chunk (#%block top ... ...)))))])

(define-s-lua-syntax-class top-level
  #:literal-sets [r-lua-ir-literals]
  
  [(#%begin ~! top:top-level ...)
   (apply append (attribute top.s-lua))]

  [(#%define ~! name val:expr)
   (let ([body (hoist-do #'val.s-lua)])
     (cond
       [(= 1 (length body))
        (let ([body (car body)])
          (syntax-parse body

            #:literal-sets [s-lua-ir-literals]
            
            ; Hoist top-level lambda definitions into local functions
            [(~or (#%function (arg ...) (#%block body ...))
                  (quote (#%function (arg ...) (#%block body ...))))
             (list
              (syntax/loc this-syntax
                (#%local (#%function name (arg ...)
                                     (#%block body ...)))))]

            ; Otherwise, treat as local binding
            [_
             (list
              (with-syntax ([val body])
                (syntax/loc this-syntax
                  (#%local [name] [val]))))]))]

       [else
        (list
         (in-s-lua
          (syntax/loc this-syntax
            (#%local [name])))
         (in-s-lua
          (let ([body (hoist-do #'val.s-lua)])
            (with-syntax ([(body ...) (drop-right body 1)]
                          [tail (last body)])
              (syntax/loc this-syntax
                (#%do
                 (#%block
                  body ...
                  (#%assign [name] [tail]))))))))]))]

  [e:expr
   (list
    (in-s-lua
     (syntax/loc this-syntax
       e.s-lua)))])

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
        (#%if cond.s-lua
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

(define-syntax-class formals
  #:attributes [[arg 1] tail]

  (pattern tail:id
           #:attr [arg 1] null)

  (pattern (arg:id ...)
           #:attr tail #f)

  (pattern (arg:id ... . tail:id)))

(define-s-lua-syntax-class val
  #:literal-sets [r-lua-ir-literals]

  [(~and datum
         (~or _:boolean
              _:number
              _:string
              _:id))
   (in-s-lua
    (syntax/loc this-syntax
      datum))]

  [(#%abs ~! args:formals body:expr)

   (let ([body (hoist-do #'body.s-lua)])
     (in-s-lua
      (with-syntax* ([ret (last body)]
                     [(body ...) (drop-right body 1)]
                     [(vararg ...)
                      (if (attribute args.tail)
                          #'(#%vararg)
                          #'())]
                     [(vararg-bind ...)
                      (if (attribute args.tail)
                          #'((#%local [args.tail] [(#%call vararg->list
                                                           vararg ...)]))
                          #'())])

        (syntax/loc this-syntax
          '(#%function (args.arg ... vararg ...)
                       (#%block
                        vararg-bind ...
                        body ...
                        (#%return ret)))))))])

(define r-lua-anf->s-lua
  (syntax-parser
    [form:module-level
     (in-s-lua
      (syntax/loc this-syntax
        form.s-lua))]))
