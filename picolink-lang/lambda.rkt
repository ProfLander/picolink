#lang racket/base

(require (for-syntax racket/base
                     picopass/base)

         racket/contract
         racket/list

         syntax/parse

         syntax-spec-v3

         picolink/language
         picolink/binding
         picolink/s-lua)

(provide (all-defined-out)
         (for-space lambda (all-defined-out))
         (for-syntax (all-defined-out)))

(define-syntax-class require-spec
  (pattern req:id
           #:with from #'req
           #:with to #'req)
  (pattern [from:id to:id]))

(define (lambda-compiler self name)
  (case name
    [(s-lua) compile/s-lua]
    [else (error "unsupported target language" name)]))

(struct lambda (requires provides source)
  #:transparent
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

  (let ([requires (collect-requires stx)]
        [provides (collect-provides stx)])
    (lambda requires provides stx)))

(syntax-spec
 (binding-class var #:binding-space lambda)
 (extension-class lambda-macro #:binding-space lambda)

 (host-interface/expression
   (#%lambda t:top-level-form)
   #:binding (scope (import t))
   #'(quote-syntax t))

 (nonterminal/exporting top-level-form
   #:binding-space lambda
   #:allow-extension lambda-macro

  (#%require ((~datum #%in) mod:id req:require-spec ...) ...)
  #:binding [(re-export req) ... ...]

  (#%provide prov:var)

  (#%define-syntax name:lambda-macro e:expr)
  #:binding (export-syntax name e)

  (#%define name:var val:expr)
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

   (#%set! ident:var expr:lambda-expr)

   (#%abs (arg:var ...) body:lambda-expr ...)
   #:binding (scope (bind arg) ... body ...)

   (#%app proc:lambda-expr arg:lambda-expr ...)

   (~> (proc arg ...)
       #'(#%app proc arg ...))))

(begin-for-syntax
  (define-language lambda-surface
    #:entry-point top-level-form
    #:terminals [id boolean number string [macro expr]]

    (top-level-form
     #:datum-literals [begin require in provide define-syntax define]
     (begin ~cut top-level-form ...)
     (require ~cut (in id require-spec ...) ...)
     (provide ~cut id ...)
     (define-syntax ~cut id macro)
     (define ~cut id expr)
     expr)

    (require-spec
     [id id]
     id)

    (expr
     #:datum-literals [λ set!]
     id
     boolean
     number
     string
     (set! ~cut id expr)
     (λ ~cut (id ...) expr ...)
     (expr expr ...)))

  (define-pass lambda-surface->ir
    (-> lambda-surface syntax?)

    (top-level-form
     (-> top-level-form syntax?)

     [(begin ~cut (~rec f:top-level-form) ...)
      #'(#%begin f ...)]

     [(require ~cut (in mod:id spec:require-spec ...) ...)
      #'(#%require (#%in mod spec ...) ...)]

     [(provide ~cut prov:id ...)
      #'(#%provide prov ...)]

     [(define-syntax ~cut name:id mac:macro)
      #'(#%define-syntax name mac)]

     [(define ~cut name:id (~rec val:expr))
      #'(#%define name val)])

    (expr
     (-> expr syntax?)

     [(set! ~cut name:id (~rec val:expr))
      #'(#%set! name val)]

     [(λ ~cut (arg:id ...) (~rec body:expr) ...)
      #'(#%abs (arg ...) body ...)]

     [((~rec proc:expr) (~rec arg:expr) ...)
      #'(#%app proc arg ...)])))

(define (compile/s-lua stx)

  (define parse-top-level
    (syntax-parser
      #:datum-literals [#%require
                        #%in
                        #%provide
                        #%define-syntax
                        #%define
                        #%begin]

      [(#%require (in _ _ ...) ...) #f]
      [(#%provide _ ...) #f]

      [(#%define-syntax name:id e:expr) #f]

      [(#%define name:id (~and %val:expr
                               (~parse val (parse-expr #'%val))))
       #'(#%local [name] [val])]

      [(#%begin e:expr ...)
       (filter-map parse-top-level (syntax-e #'(e ...)))]

      [e:expr (parse-expr #'e)]))

  (define parse-expr
    (syntax-parser
      #:datum-literals [#%set! #%abs #%app]

      [bool:boolean #'bool]
      [num:number #'num]
      [str:string #'str]
      [ident:id #'ident]

      [(#%set! ident:id (~and %val:expr
                              (~parse val (parse-expr #'%val))) )
       #'(#%assign [ident] [val])]

      [(#%abs (arg:id ...)
                 (~and %body:expr (~parse body (parse-expr #'%body))) ...
                 (~and %ret:expr (~parse ret (parse-expr #'%ret))))
       #'(#%function (arg ...)
                     (#%block body ... (#%return ret)))]

      [(#%app
        (~and %proc:expr
              (~parse proc (parse-expr #'%proc)))
        (~and %arg:expr
              (~parse arg (parse-expr #'%arg)))
        ...)
       #'(#%call proc arg ...)]))

  (make-s-lua (parse-top-level stx)))
