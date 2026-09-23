#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/contract
         racket/list

         syntax/parse
         syntax/id-table
         syntax/id-set

         syntax-spec-v3

         picolink/input
         picolink/output/s-lua)

(provide (all-defined-out)
         (for-space lambda (all-defined-out))
         (for-syntax (all-defined-out)))

(define (lambda-collect-require self stx)
  (syntax-parse stx
    #:datum-literals [#%require #%in]
    [(#%require (#%in mod:id req:id ...) ...)
     (cons this-syntax
           (for/fold ([acc (make-immutable-free-id-table)])
                     ([entry (in-list (syntax-e
                                       #'((mod req ...) ...)))])
             (syntax-parse entry
               [(mod:id req:id ...)
                (free-id-table-set acc #'mod
                                   (immutable-free-id-set
                                    (syntax-e #'(req ...))))])))]
    [_ #f]))

(define (lambda-collect-provide self stx)
  (syntax-parse stx
    #:datum-literals [#%provide]
    [(#%provide prov:id ...)
     (immutable-free-id-set (attribute prov))]
    [_ #f]))

(define (lambda-compiler self name)
  (case name
    [(s-lua) compile/s-lua]
    [else (error "unsupported backend" name)]))

(struct lambda (source)
  #:transparent
  #:methods gen:input

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

  (#%intrinsic name:var)
  #:binding (export name)

  (#%require ((~datum #%in) mod:id req:var ...) ...)
  #:binding [(export req) ... ...]

  (#%provide prov:var)

  (#%define name:var val:expr)
  #:binding (export name)

  (#%begin top:top-level-form ...)
  #:binding [(re-export top) ...]

  e:expr)

 (nonterminal expr
   #:binding-space lambda
   #:allow-extension lambda-macro

   b:boolean
   n:number
   s:string
   v:var

   (#%lambda (arg:var ...) body:expr ...)
   #:binding (scope (bind arg) ... body ...)

   (#%app proc:expr arg:expr ...)

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

(define-lambda-syntax-parser define
  [(_ ident value)
   #'(#%define ident value)])

(define-lambda-syntax-parser λ
  [(_ (arg ...) body ...)
   #'(#%lambda (arg ...) body ...)])

(define (compile/s-lua stx)

  (define parse-top-level
    (syntax-parser
      #:datum-literals [#%require #%in #%provide #%define #%begin]

      [(#%intrinsic _:id) #f]
      [(#%require (in _:id _:id ...) ...) #f]
      [(#%provide _:id ...) #f]

      [(#%define name:id (~and %val:expr
                               (~parse val (parse-expr #'%val))))
       #'(#%local [name] [val])]

      [(#%begin e:expr ...)
       (filter-map parse-top-level (syntax-e #'(e ...)))]

      [(~and %e:expr
             (~parse e (parse-expr #'%e)))
       #'(print e)]))

  (define parse-expr
    (syntax-parser
      #:datum-literals [#%lambda #%app]

      [bool:boolean #'bool]
      [num:number #'num]
      [str:string #'str]
      [ident:id #'ident]

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
       #'(proc arg ...)]))

  (make-s-lua (parse-top-level stx)))

(define/contract (make-lambda stx)
  (-> syntax? lambda?)
  (lambda stx))
