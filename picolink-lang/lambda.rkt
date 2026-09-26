#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/contract
         racket/function
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

  (#%define name:var val:expr)
  #:binding (export name)

  (#%begin top:top-level-form ...)
  #:binding [(re-export top) ...]

  e:expr)

 (nonterminal/exporting require-spec
   req:var
   #:binding (export req)

   [from:id to:var]
   #:binding (export to))

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

      [(#%require (in _ _ ...) ...) #f]
      [(#%provide _ ...) #f]

      [(#%define name:id (~and %val:expr
                               (~parse val (parse-expr #'%val))))
       #'(#%local [name] [val])]

      [(#%begin e:expr ...)
       (filter-map parse-top-level (syntax-e #'(e ...)))]

      [(~and %e:expr
             (~parse e (parse-expr #'%e)))
       #'(#%call print e)]))

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
       #'(#%call proc arg ...)]))

  (make-s-lua (parse-top-level stx)))

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

    (make-module-requires (parse stx)))

  (define (collect-provides stx)
    (define parse
      (syntax-parser
        #:datum-literals [#%begin #%provide]

        [(#%begin form ...)
         (map set-union (filter-map parse (attribute form)))]

        [(#%provide prov:id ...)
         (list->set (map make-binding (attribute prov)))]

        [_ #f]))

    (make-module-provides (parse stx)))

  (let ([requires (collect-requires stx)]
        [provides (collect-provides stx)])
    (lambda requires provides stx)))
