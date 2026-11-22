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

(define current-debug
  (make-parameter #f))

(define (r-lua->s-lua stx)

  (let ([debug (current-debug)])

    (define anf (r-lua->r-lua-anf stx))

    (when debug
      (displayln "anf:")
      (println (syntax->datum anf))
      (displayln ""))

    (define s-lua (r-lua-anf->s-lua anf))

    (when debug
      (displayln "s-lua:")
      (println (syntax->datum s-lua))
      (displayln ""))

    s-lua))

(define (sink f stx)
  (syntax-parse stx
    #:literal-sets [s-lua-ir-literals]
    [(#%chunk (~and %block (~parse block (sink f #'%block))))
     (in-s-lua
      (syntax/loc this-syntax
        (#%chunk block)))]

    [(#%block body ...
              (~and %tail
                    (~parse tail (sink f #'%tail))))
     (in-s-lua
      (syntax/loc this-syntax
        (#%block body ... tail)))]

    [(#%do (~and %block
                 (~parse block (sink f #'%block))))
     (in-s-lua
      (syntax/loc this-syntax
        (#%do block)))]

    [(#%if cond
           (~and (#%then %then)
                 (~parse then (sink f #'%then)))
           (~and (#%else %else)
                 (~parse else (sink f #'%else))))
     (in-s-lua
      (syntax/loc this-syntax
        (#%if cond
              (#%then then)
              (#%else else))))]

    [(~and %form (~parse form (f #'%form)))
     (in-s-lua
      (syntax/loc this-syntax
        form))]))

(define (sink-return stx)
  (sink (λ (stx)
          (quasisyntax/loc stx
            (#%return #,stx)))
        stx))

(define (sink-assign vals stx)
  (sink (λ (stx)
          (quasisyntax/loc stx
            (#%assign [#,@vals] [#,stx])))
        stx))

(define (sink-local vals stx)

  (syntax-parse stx
    #:literal-sets [s-lua-ir-literals]
    [(#%chunk (~and %block (~parse (block) (sink-local vals #'%block))))
     (list
      (in-s-lua
       (syntax/loc this-syntax
         (#%chunk block))))]

    [(#%block body ...
              (~and %tail
                    (~parse (tail ...) (sink-local vals #'%tail))))
     (list
      (in-s-lua
       (syntax/loc this-syntax
         (#%block body ... tail ...))))]

    [(#%do
      (#%block body
               ...
               (~and %tail
                     (~parse (temp ...) (generate-temporaries vals))
                     (~parse tail (sink-assign #'(temp ...) #'%tail)))))

     (list
      (in-s-lua
       (quasisyntax/loc this-syntax
         (#%local [temp ...])))
      (in-s-lua
       (quasisyntax/loc this-syntax
         (#%do
          (#%block
           body ...
           tail))))
      (in-s-lua
       (quasisyntax/loc this-syntax
         (#%local [#,@vals] [temp ...]))))]

    [(#%if (~parse (temp ...) (generate-temporaries vals))
           cond
           (~and (#%then %then)
                 (~parse then (sink-assign #'(temp ...) #'%then)))
           (~and (#%else %else)
                 (~parse else (sink-assign #'(temp ...) #'%else))))
     (list
      (in-s-lua
       (quasisyntax/loc this-syntax
         (#%local [temp ...])))
      (in-s-lua
       (syntax/loc this-syntax
         (#%if cond
               (#%then then)
               (#%else else))))
      (in-s-lua
       (quasisyntax/loc this-syntax
         (#%local [#,@vals] [temp ...]))))]

    [form
     (list
      (in-s-lua
       (quasisyntax/loc this-syntax
         (#%local [#,@vals] [form]))))]))

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
    (with-syntax ([(top ...) (apply append (attribute top.s-lua))])
      (syntax/loc this-syntax
       (#%chunk (#%block top ...)))))])

(define-s-lua-syntax-class top-level
  #:literal-sets [r-lua-ir-literals]
  
  [(#%begin ~! top:top-level ...)
   (apply append (attribute top.s-lua))]

  [(#%define ~! name %val:expr)

   (syntax-parse (sink-assign #'(name) #'%val.s-lua)
     #:literal-sets [s-lua-ir-literals]

     [(#%assign [_] [(quote (#%function args body))])
      (list
       (syntax/loc this-syntax
         (#%local (#%function name args body))))]

     [_
      (list
       (syntax/loc this-syntax
         (#%local [name]))
       this-syntax)])]

  [e:expr
   (list
    (in-s-lua
     (syntax/loc this-syntax
       e.s-lua)))])

(define hoist-do
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]

    [(#%do (#%block body ...))
     #'(body ...)]

    [body
     #'(body)]))

(define-s-lua-syntax-class expr
  #:literal-sets [r-lua-ir-literals]

  [(#%set! ~! ident expr:expr)
   (sink-assign #'(ident) #'expr.s-lua)]

  [(#%let-values ~! ((~and [(name:id ...) %val:expr]
                           (~parse (val ...)
                                   (sink-local #'(name ...)
                                               #'%val.s-lua)))
                     ...)
                 body:expr)
   (in-s-lua
    (with-syntax ([(body ...) (hoist-do #'body.s-lua)])
      (syntax/loc this-syntax
        (#%do
         (#%block
          val ... ...
          body ...)))))]

  [(#%letrec-values ~! ((~and [(name:id ...) %val:expr]
                              (~parse val (sink-assign #'(name ...)
                                                       #'%val.s-lua)))
                        ...)
                    body:expr)
   (in-s-lua
    (with-syntax ([(body ...) (hoist-do #'body.s-lua)])
      (syntax/loc this-syntax
        (#%do
         (#%block
          (#%local [name ... ...])
          val ...
          body ...)))))]

  [(#%begin ~! body:expr ... tail:expr)
   (in-s-lua
    (with-syntax ([(body ...) (attribute body.s-lua)]
                  [(tail ...) (hoist-do (attribute tail.s-lua))])
      (syntax/loc this-syntax
        (#%do
         (#%block body ... tail ...)))))]

  [(#%app ~! expr:expr ...+)
   (in-s-lua
    (syntax/loc this-syntax
      (#%call expr.s-lua ...)))]

  [(#%if ~! cond:expr
         then:expr
         else:expr)
   
   (with-syntax ([(then ...) (hoist-do #'then.s-lua)]
                 [(else ...) (hoist-do #'else.s-lua)])

     (in-s-lua
      (syntax/loc this-syntax
        (#%if cond.s-lua
              (#%then (#%block then ...))
              (#%else (#%block else ...))))))]

  [(#%s-lua ~! form)
   (in-s-lua
    (not-in-r-lua
     (syntax/loc this-syntax
       form)))]

  [v:val
   (in-s-lua
    (syntax/loc this-syntax
      v.s-lua))])

(define-splicing-syntax-class formals
  #:attributes [(arg 1)
                tail
                tail-bind]

  (pattern [(~datum :) tail:id tail-bind:id]
           #:attr [arg 1] null)

  (pattern tail:id
           #:attr [arg 1] null
           #:attr tail-bind #'list)

  (pattern (arg:id ... . (~or [(~datum :) tail:id %tail-bind:id]
                              tail:id))
           #:attr tail-bind (if (attribute %tail-bind)
                                (attribute %tail-bind)
                                #'list))

  (pattern (arg:id ...)
           #:attr tail #f
           #:attr tail-bind #f))

(define-s-lua-syntax-class val
  #:literal-sets [r-lua-ir-literals]

  [(~or _:boolean
        _:number
        _:string
        _:id)
   (in-s-lua this-syntax)]

  [(#%abs ~! args:formals body:expr)

   (define (replace-vararg name stx)
     (syntax-parse stx
       [ident:id
        #:when (datum=? #'ident name)
        (syntax/loc this-syntax
          #%vararg)]

       [(form ...)
        (datum->syntax this-syntax
                       (map (λ (form)
                              (replace-vararg name form))
                            (attribute form))
                       this-syntax
                       this-syntax)]

       [_ this-syntax]))

   (let ([body #'body.s-lua]
         [is-vararg-values (and (attribute args.tail)
                                (datum=? (attribute args.tail-bind)
                                         #'#%values))])

     (in-s-lua
      (let* ([body         (sink-return
                            (if is-vararg-values
                                (replace-vararg (attribute args.tail)
                                                body)
                                body))]

             [vararg       (if (attribute args.tail)
                               #'(#%vararg)
                               #'())]

             [vararg-binds (if (and (attribute args.tail)
                                    (not is-vararg-values))

                               #`((#%local [args.tail]
                                           [(#%call args.tail-bind
                                                    #,@vararg)]))
                               #'())])

        (with-syntax* ([(vararg ...) vararg]

                       [(body ...)   (hoist-do body)]

                       [body         (quasisyntax/loc this-syntax
                                       (#%block
                                        #,@vararg-binds
                                        body ...))])

          (syntax/loc this-syntax
            '(#%function (args.arg ... vararg ...)
                         body))))))])

(define r-lua-anf->s-lua
  (syntax-parser
    [form:module-level
     (in-s-lua
      (syntax/loc this-syntax
        form.s-lua))]))
