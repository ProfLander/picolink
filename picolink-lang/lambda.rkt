#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/contract
         racket/syntax
         racket/list
         racket/struct
         racket/sequence

         syntax-spec-v3

         picopass/base

         picolink/language
         picolink/binding
         (except-in picolink/s-lua s-lua)
         picolink/s-lua/grammar)

(provide (all-defined-out)
         (for-space lambda (all-defined-out))
         (for-syntax (all-defined-out)))

; Surface syntax

(define-language lambda-surface
  #:entry-point top-level-form
  #:terminals [id
               [var id]
               [lambda-macro id]
               boolean
               number
               string
               expr]

  (top-level-form
   #:datum-literals [begin require in provide define-syntax define]
   (begin ~cut top-level-form ...)
   (require ~cut (in id require-spec ...) ...)
   (provide ~cut var ...)
   (define-syntax ~cut lambda-macro macro-clause ...+)
   (define ~cut var lambda-expr)
   lambda-expr)

  (macro-clause
   #:datum-literals [syntax]
   (expr
    expr ...
    top-level-form))

  (require-spec
   [id var]
   id)

  (lambda-expr
   #:datum-literals [let set! λ]

   var

   boolean
   number
   string

   (let ~cut ([id lambda-expr] ...)
     lambda-expr ...)

   (set! ~cut id lambda-expr)

   (λ ~cut (var ...) lambda-expr ...)

   (lambda-expr ...+)))

(define-pass lambda-surface->ir
  #;(-> lambda-surface #%lambda)
  (-> lambda-surface syntax?)

  (top-level-form
   #;(-> top-level-form top-level-form)
   (-> top-level-form syntax?)

   [(begin ~cut (~rec f:top-level-form) ...)
    #'(#%begin f ...)]

   [(require ~cut (in mod:id spec:require-spec ...) ...)
    #'(#%require (#%in mod spec ...) ...)]

   [(provide ~cut prov:var ...)
    #'(#%provide prov ...)]

   [(define-syntax ~cut name:lambda-macro (~rec clause:macro-clause) ...+)
    #'(#%define-syntax name (syntax-parser clause ...))]

   [(define ~cut name:var (~rec val:lambda-expr))
    #'(#%define name val)])

  (macro-clause
   (-> macro-clause syntax?)
   [(pat:expr expr:expr ... (~rec form:top-level-form))
    #'[pat
       expr ...
       (syntax form)]])

  (lambda-expr
   #;(-> lambda-expr lambda-expr)
   (-> lambda-expr syntax?)

   [(let ~cut ([name:id (~rec val:lambda-expr)] ...)
      (~rec body:lambda-expr)
      ...)
    #'(#%let ([name val] ...)
             body ...)]

   [(set! ~cut name:id (~rec val:lambda-expr))
    #'(#%set! name val)]

   [(λ ~cut (arg:var ...) (~rec body:lambda-expr) ...)
    #'(#%abs (arg ...) body ...)]

   [((~rec expr:lambda-expr) ...+)
    #'(expr ...)]))

; IR

(define (lambda-compiler self name)
  (case name
    [(s-lua) compile/s-lua]
    [else (error "unsupported target language" name)]))

(struct lambda (requires provides source)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 'lambda)
      (λ (self) (list (cons 'requires (lambda-requires self))
                      (cons 'provides (lambda-provides self))
                      (list 'source
                            (syntax->datum
                             (lambda-source self)))))))]

  #:methods gen:language

  [(define (requires self)
     (lambda-requires self))

   (define (provides self)
     (lambda-provides self))

   (define (source self)
     (lambda-source self))

   (define compiler lambda-compiler)])

(define/contract (make-lambda stx)
  (-> syntax? lambda?)

  (define-syntax-class require-spec
    (pattern req:id
             #:with from #'req
             #:with to #'req)
    (pattern [from:id to:id]))

  (define (collect-requires stx)
    (define parse
      (syntax-parser
        #:datum-literals [#%begin #%require #%in]

        [(#%begin form ...)
         (apply append (filter-map parse (attribute form)))]

        [(#%require (#%in mod:id req:require-spec ...) ...)
         (let ([req-stx this-syntax])
           (for/list ([entry (in-list (syntax-e #'((mod req ...) ...)))])
             (syntax-parse entry
               [(mod:id req:require-spec ...)
                (cons #'mod
                      (map (syntax-parser
                             [req:require-spec
                              (make-module-require req-stx
                                                   #'req.from
                                                   #'req.to)])
                           (attribute req)))])))]

        [_ #f]))

    (let ([requires (parse stx)])
      (make-module-requires requires)))

  (define (collect-provides stx)
    (define parse
      (syntax-parser
        #:datum-literals [#%begin #%provide]

        [(#%begin form ...)
         (apply append (filter-map parse (attribute form)))]

        [(#%provide prov:id ...)
         (map make-binding (attribute prov))]

        [_ #f]))

    (let ([provides (parse stx)])
      (make-module-provides provides)))

  (let* ([stx (lambda-surface->ir stx)]
         [requires (collect-requires stx)]
         [provides (collect-provides stx)])
    (lambda requires provides stx)))

(derive-language #%lambda
  #:entry-point top-level-form

  (syntax-spec
   (binding-class var #:binding-space lambda)
   (extension-class lambda-macro #:binding-space lambda)

   (host-interface/expression
     (#%lambda-spec t:top-level-form)
     #:binding (scope (import t))
     #'(quote-syntax t))

   (nonterminal/exporting top-level-form
     #:binding-space lambda
     #:allow-extension lambda-macro

     (#%require ((~datum #%in) mod:id req:require-spec ...) ...)
     #:binding [(re-export req) ... ...]

     (#%provide prov:var ...)

     (#%define-syntax name:lambda-macro e:expr)
     #:binding (export-syntax name e)

     (#%define name:var val:lambda-expr)
     #:binding (export name)

     (#%begin top:top-level-form ...)
     #:binding [(re-export top) ...]

     e:lambda-expr)

   (nonterminal/exporting require-spec
     req:var
     #:binding (export req)

     [from:id to:var]
     #:binding (export to))

   (nonterminal lambda-expr
     #:binding-space lambda
     #:allow-extension lambda-macro

     b:boolean
     n:number
     s:string
     v:var

     (#%let ([ident:var expr:lambda-expr] ...)
            body:lambda-expr ...)
     #:binding (scope (bind ident) ... body ...)

     (#%set! ident:var expr:lambda-expr)

     (#%abs (arg:var ...) body:lambda-expr ...)
     #:binding (scope (bind arg) ... body ...)

     (#%app expr:lambda-expr ...+)

     (~> (arg ...+)
         #'(#%app arg ...)))))

; A-Normal Form

(define-language #%lambda-anf
  #:extends #%lambda

  (lambda-expr

   (- var

      boolean
      number
      string

      (#%let ([var lambda-expr] ...) lambda-expr ...)
      (#%abs (var ...) lambda-expr ...)
      (#%app lambda-expr ...+))

   (+ val
      (#%let ([id lambda-expr] ...) lambda-expr ...)
      (#%app val ...+)))

  (val
   #:datum-literals+ [#%abs]
   (+ var

      boolean
      number
      string

      (#%abs (var ...) lambda-expr ...))))

(define (#%lambda-exprs->binds+atom exprs)
  (for/fold ([binds null]
             [atoms null])
            ([expr (in-list exprs)])
    (let-values ([(binds* atom)
                  (#%lambda->binds+atom expr)])
      (values (append binds binds*)
              (append atoms (list atom))))))

; ANF Conversion

(define #%lambda->binds+atom
  (syntax-parser
    #:datum-literals [#%begin
                      #%require
                      #%provide
                      #%define
                      #%define-syntax
                      #%let
                      #%set!
                      #%abs
                      #%app]

    [(~or _:id
          _:boolean
          _:string
          _:number
          (#%require _ ...)
          (#%provide _ ...)
          (#%define-syntax _ ...))
     (values null this-syntax)]

    [(#%begin expr ...)
     (with-syntax ([(expr ...) (map #%lambda->#%lambda-anf
                                     (attribute expr))])
       (values null #'(#%begin expr ...)))]

    [(#%define ident expr)
     (with-syntax ([expr (#%lambda->#%lambda-anf #'expr)])
       (values null #'(#%define ident expr)))]

    [(#%let ([bind expr] ...)
            body ...)

     (define-values (expr-binds expr-atoms)
       (#%lambda-exprs->binds+atom (attribute expr)))

     (with-syntax ([(expr-atom ...) expr-atoms]
                   [(body ...) (map #%lambda->#%lambda-anf
                                    (attribute body))])
       (values expr-binds
               #'(#%let ([bind expr-atom] ...)
                        body ...)))]

    [(#%set! ident expr)
     (with-syntax ([expr (#%lambda->#%lambda-anf #'expr)])
       (values null #'(#%set! ident expr)))]

    [(#%abs (arg ...) body ...)
     (with-syntax ([(body ...) (map #%lambda->#%lambda-anf
                                    (attribute body))])
       (values null
               #'(#%abs (arg ...)
                        body ...)))]

    [(#%app expr ...)

     (define-values (expr-binds expr-atoms)
       (#%lambda-exprs->binds+atom (attribute expr)))

     (with-syntax ([atom (generate-temporary)]
                   [(expr-atom ...) expr-atoms])
       (values (append expr-binds
                       (list (cons #'atom #'(#%app expr-atom ...))))
               #'atom))]))

(define (binds+atom->#%lambda-anf binds atom)
  (if (pair? binds)
      (let ([is-ident (identifier? atom)])
        (let ([binds (if is-ident
                         (drop-right binds 1)
                         binds)]
              [atom (if is-ident
                        (cdr (last binds))
                        atom)])
          (for/foldr ([stx atom])
                     ([pair (in-list binds)])
            (with-syntax ([bind (car pair)]
                          [val (cdr pair)])
              #`(#%let ([bind val])
                       #,stx)))))
      atom))

(define (#%lambda->#%lambda-anf stx)
  (define-values (binds atom) (#%lambda->binds+atom stx))
  (binds+atom->#%lambda-anf binds atom))

; S-Lua Compiler

(define (unimplemented)
  (error "unimplemented"))

(define (hoist-do stx)
  (for/fold ([acc null])
            ([body (in-syntax stx)])
    (append acc
            (syntax-parse body
              #:datum-literals [#%do #%block]
              [(#%do (#%block body ...))
               (attribute body)]
              [_ (list this-syntax)]))))

(define-pass #%lambda-anf->s-lua
  (-> #%lambda-anf s-lua)
  
  (top-level-form->block
   (-> top-level-form block)
   [(#%begin top:top-level-form ...)
    (let ([tops
           (filter-map
            (syntax-parser
              #:datum-literals [#%require
                                #%provide
                                #%define-syntax]
              [((~or #%require
                     #%provide
                     #%define-syntax)
                _ ...)
               #f]
              [_ (rec/top-level-form this-syntax)])
            (syntax-e #'(top ...)))])
      (syntax-parse tops
        #:datum-literals [#%do #%block]
        [[stmt ... (#%do (#%block body ...))]
         #'(#%block stmt ... body ...)]
        [(top ...)
         #'(#%block top ...)]))])

  (top-level-form->statement
   (-> top-level-form statement)
   [(#%define name:var (~rec val:lambda-expr))
    #'(#%local [name] [val])]

   [(~rec e:lambda-expr) #'e])

  (top-level-form->*
   #;(-> top-level-form *)
   (-> top-level-form block)
   [(#%require (#%in mod:id req:require-spec ...) ...)
    (unimplemented)]

   [(#%provide prov:var ...)
    (unimplemented)]

   [(#%define-syntax name:lambda-macro e:expr)
    (unimplemented)])

  (require-spec->*
   #;(-> require-spec *)
   (-> require-spec block)

   [req:var
    (unimplemented)]
   [[from:id to:var]
    (unimplemented)])

  (lambda-expr->expr
   (-> lambda-expr expr)

   [(~rec v:val)
    #'v]

   [(#%app (~rec expr:val) ...+)
    #'(#%call expr ...)])
  
  (lambda-expr->statement
   (-> lambda-expr statement)

   [(#%set! ident:var (~rec expr:lambda-expr))
    #'(#%assign [ident] [expr])]

   [(#%let ([name:id (~rec val:lambda-expr)] ...)
           (~rec body:lambda-expr) ...)
    (with-syntax ([(body ...) (hoist-do #'(body ...))])
      #'(#%do
         (#%block
          (#%local [name ...] [val ...])
          body ...)))])

  (val
   (-> val expr)

   [v:var #'v]
   [b:boolean #'b]
   [n:number #'n]
   [s:string #'s]

   [(#%abs (arg:var ...)
           (~rec body:lambda-expr) ...)
    (let ([body (hoist-do #'(body ...))])
      (with-syntax ([(body ...) (drop-right body 1)]
                    [ret (last body)])
        #''(#%function (arg ...)
                       (#%block
                        body ...
                        (#%return ret)))))]))

(define (expand-syntax-spec stx)
  (parameterize ([current-namespace
                  (variable-reference->namespace
                   (#%variable-reference))])
    (syntax-parse (expand-syntax stx)
      #:datum-literals [begin
                         define-values
                         #%expression
                         quote-syntax]
      [(begin
         (define-values _ ...)
         (#%expression
          (quote-syntax
           stx)))
       #'stx])))

(define (compile/s-lua stx)
  (let* ([stx (expand-syntax-spec #`(#%lambda-spec #,stx))]
         [stx (#%lambda->#%lambda-anf stx)]
         [stx (#%lambda-anf->s-lua stx)])
    (make-s-lua #`(#%chunk #,stx))))
