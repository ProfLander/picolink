#lang racket/base

(require syntax/parse
         picolink/syntax
         picolink/ee-lib
         picolink/parser
         picolink/intrinsic
         picolink/intrinsic/var
         (for-template picolink/r-lua/ir))

(provide (all-defined-out))

(define-literal-set r-lua-surface-literals
  #:datum-literals [begin
                     require
                     in
                     provide
                     define
                     define-macro
                     let
                     set!
                     λ
                     if
                     void]
  [])

(struct/cross-phase r-lua-var [])
(struct/cross-phase r-lua-macro [trx])

(define (supported-var? binding)
  (or (r-lua-var? binding)
      (intrinsic-var? binding)))

(define (check-var ident)

  (define binding (lookup ident))

  (unless binding
    (parameterize ([error-print-context-length 0])
      (raise-syntax-error #f
                          "unbound variable"
                          ident)))

  (unless (supported-var? binding)
    (parameterize ([error-print-context-length 0])
      (raise-syntax-error #f
                          (format "unsupported variable binding: ~a"
                                  binding)
                          ident)))

  ident)

(define require?
  (syntax-parser
    #:literal-sets [r-lua-surface-literals]
    [(require _ ...) #t]
    [_ #f]))

(define provide?
  (syntax-parser
    #:literal-sets [r-lua-surface-literals]
    [(provide _ ...) #t]
    [_ #f]))

(define/hygienic (r-lua/top-level stx)
  #:definition

  (syntax-parse stx
    #:literal-sets [r-lua-surface-literals]

    [(begin body ...)

     (define-values (requires provides bodies)
       (lift-requires+provides require? provide? (attribute body)))

     (for ([req (in-list requires)])
       (r-lua/top-level req))

     (define bodies*
       (for/list ([body (in-list bodies)])
         (r-lua/top-level body)))

     (for ([prov (in-list provides)])
       (r-lua/top-level prov))

     (with-syntax ([(body ...) bodies*])
       (syntax/loc this-syntax
         (#%begin body ...)))]

    [(require (in mod
                  (~cata req (require! this-syntax #'mod _))
                  ...)
              ...)
     (void)]

    [(provide (~cata prov (provide! this-syntax _))
              ...)
     (void)]

    [(define name (~cata expr r-lua/expr))
     (bind! #'name (r-lua-var))
     (syntax/loc this-syntax
       (#%define name expr))]

    [(define-macro name expr)
     (bind! #'name (r-lua-macro (eval-transformer (r-lua/macro #'expr))))
     (syntax/loc this-syntax
       (#%define-macro name expr))]

    [(~cata e r-lua/expr)
     (syntax/loc this-syntax
       e)]))

(define/hygienic-expression-parser r-lua/macro
  #:literal-sets [r-lua-surface-literals]

  [(pat body ...)
   #'(syntax-parser
       [pat body ...])])

(define/hygienic (r-lua/expr stx)
  #:expression

  (syntax-parse stx
    #:literal-sets [r-lua-surface-literals]

    [(~cata ident:id check-var)
     (syntax/loc this-syntax
       ident)]

    [b:boolean
     (syntax/loc this-syntax
       b)]

    [n:number
     (syntax/loc this-syntax
       n)]

    [s:string
     (syntax/loc this-syntax
       s)]

    [(begin (~cata body r-lua/expr) ...)
     #'(#%begin body ...)]

    [(let _ ...)
     (syntax-parser/scope
      scope
      [(_ ([name (~cata val r-lua/expr)] ...)
          body ...)

       (for ([name (in-list (attribute name))])
         (bind! name (r-lua-var)))

       (with-syntax ([(body ...) (map r-lua/expr (attribute body))])
         (syntax/loc this-syntax
           (#%let ([name val] ...)
                  (#%begin body ...))))])]

    [(set! (~cata name check-var)
           (~cata val r-lua/expr))
     (syntax/loc this-syntax
       (#%set! name val))]

    [(λ _ ...)
     (syntax-parser/scope
      scope
      [(_ ((~cata arg (bind! _ (r-lua-var))) ...)
          (~cata body r-lua/expr) ...)
       (syntax/loc this-syntax
         (#%abs (arg ...)
                (#%begin body ...)))])]

    [(if (~cata cond r-lua/expr)
         (~cata then r-lua/expr)
         (~cata else r-lua/expr))
     #'(#%if cond then else)]

    [(void)
     #'#%void]

    [(ident:id arg ...)
     #:do [(define mac (lookup #'ident r-lua-macro?))]
     #:when mac
     (r-lua/expr
      (apply-as-transformer (r-lua-macro-trx mac)
                            #'ident
                            'expression
                            #'(ident arg ...)))]

    [((~cata expr r-lua/expr) ...+)
     (syntax/loc this-syntax
       (#%app expr ...))]))
