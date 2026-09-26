#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/contract
         racket/list
         racket/set

         syntax/parse
         syntax/id-table
         syntax/id-set

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

(define (lambda-collect-require self stx)
  (syntax-parse stx
    #:datum-literals [#%require #%in]
    [(#%require (#%in mod:id req:require-spec ...) ...)
     (cons this-syntax
           (for/fold ([acc (hash)])
                     ([entry (in-list (syntax-e #'((mod req ...) ...)))])
             (syntax-parse entry
               [(mod:id req:require-spec ...)
                (hash-set acc (make-binding #'mod)
                          (list->set
                           (for/list ([req (in-list (attribute req))])
                             (syntax-parse req
                               [req:require-spec
                                (cons (binding #'req.from)
                                      (binding #'req.to))]))))])))]
    [_ #f]))

(define (lambda-collect-provide self stx)
  (syntax-parse stx
    #:datum-literals [#%provide]
    [(#%provide prov:id ...)
     (list->set (map make-binding (attribute prov)))]
    [_ #f]))

(define (lambda-compiler self name)
  (case name
    [(s-lua) compile/s-lua]
    [else (error "unsupported target language" name)]))

(struct lambda (source)
  #:transparent
  #:methods gen:language

  [(define (source self)
     (lambda-source self))

   (define collect-require lambda-collect-require)
   (define collect-provide lambda-collect-provide)
   (define compiler lambda-compiler)])

(syntax-spec
 (binding-class var #:binding-space lambda)
 (extension-class lambda-macro #:binding-space lambda)

 (host-interface/expression
   (lambda/check-binds t:top-level-form ...)
   #:binding (scope (import t) ...)
   #'#'(#%begin t ...))

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

   (#%lambda (arg:var ...) body:lambda-expr ...)
   #:binding (scope (bind arg) ... body ...)

   (#%app proc:lambda-expr arg:lambda-expr ...)

   (~> (proc arg ...)
       #'(#%app proc arg ...))))

(define-syntax define-lambda-syntax
  (syntax-parser
    [(_ name body ...)
     #'(define-dsl-syntax name lambda-macro
         body ...)]))

(define-syntax define-lambda-syntax-parser
  (syntax-parser
    [(_ name body ...)
     #'(define-lambda-syntax name
         (syntax-parser body ...))]))

(begin-for-syntax
  (define local-expand-expr (nonterminal-expander lambda-expr)))

(define-lambda-syntax-parser begin
  [(_ body ...)
   #'(#%begin body ...)])

(define-lambda-syntax-parser require
  #:datum-literals [in]
  [(_ (in mod req ...) ...)
   #'(#%require (#%in mod req ...) ...)])

(define-lambda-syntax-parser provide
  [(_ prov ...)
   #'(#%provide prov ...)])

(define-lambda-syntax-parser define-syntax
  [(_ ident clauses ...)
   #'(#%define-syntax
      ident
      (syntax-parser clauses ...))])

(define-lambda-syntax-parser define
  [(_ ident value)
   (with-syntax ([value (local-expand-expr #'value)])
     #'(#%define ident value))])

(define-lambda-syntax-parser set!
  [(_ ident expr)
   #'(#%set! ident expr)])

(define-lambda-syntax-parser λ
  [(_ (arg ...) body ...)
   #'(#%lambda (arg ...) body ...)])

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
      #:datum-literals [#%set! #%lambda #%app]

      [bool:boolean #'bool]
      [num:number #'num]
      [str:string #'str]
      [ident:id #'ident]

      [(#%set! ident:id (~and %val:expr
                              (~parse val (parse-expr #'%val))) )
       #'(#%assign [ident] [val])]

      [(#%lambda (arg:id ...)
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

(define/contract (make-lambda stx)
  (-> syntax? lambda?)
  (lambda stx))
