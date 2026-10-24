#lang racket/base

(require syntax/parse
         picolink/syntax
         picolink/ee-lib
         picolink/parser
         picolink/module/scopes
         picolink/intrinsic/var
         (for-template picolink/r-lua/ir
                       picolink/s-lua/ir))

(provide (all-defined-out))

(define-literal-set r-lua-surface-literals
  #:datum-literals [begin
                     require
                     provide
                     define
                     define-macro
                     let
                     letrec
                     set!
                     λ
                     if
                     void
                     #%intrinsic]
  [])

(struct/cross-phase r-lua-var [])
(struct/cross-phase r-lua-macro [trx])

(define (supported-var? binding)
  (or (r-lua-var? binding)
      (intrinsic-var? binding)))

(define (check-var ident)

  (define binding (lookup-in-module ident))

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

(define/hygienic (r-lua/top-level stx)
  #:definition

  (syntax-parse stx
    #:literal-sets [r-lua-surface-literals]

    [(begin ~! body ...)

     (define-values (requires provides bodies)
       (lift-requires+provides require? provide? (attribute body)))

     (for ([req (in-list requires)])
       (r-lua/top-level req))

     (define bodies*
       (for/list ([body (in-list bodies)]
                  #:do [(define body* (r-lua/top-level body))]
                  #:unless (void? body*))
         body*))

     (for ([prov (in-list provides)])
       (r-lua/top-level prov))

     (in-r-lua
      (with-syntax ([(body ...) bodies*])
        (syntax/loc this-syntax
          (#%begin body ...))))]

    [(require ~! (~var _ (bind-require this-syntax)))
     (void)]

    [(provide ~! (~var _ (bind-provide this-syntax)))
     (void)]

    [(define ~! name expr)

     (bind-in-module! #'name (r-lua-var))

     (in-r-lua
      (with-syntax ([expr (r-lua/expr #'expr)])
        (syntax/loc this-syntax
          (#%define name expr))))]

    [(define-macro ~! name expr ...)
     (bind-in-module! #'name (r-lua-macro
                              (eval-transformer
                               (r-lua/macro
                                #'(expr ...)))))
     (void)]

    [(ident:id arg ...)
     #:do [(define mac (lookup-in-module #'ident r-lua-macro?))]
     #:when mac
     #:cut
     (r-lua/top-level
      (apply-as-transformer (r-lua-macro-trx mac)
                            #'ident
                            'definition
                            this-syntax))]

    [(~cata e r-lua/expr)
     (syntax/loc this-syntax
       e)]))

(define r-lua/macro
  (syntax-parser
    #:literal-sets [r-lua-surface-literals]

    [([pat body ... tail] ...)
     (syntax/loc this-syntax
       (syntax-parser
         [pat
          body ...
          (datum->syntax this-syntax
                         (syntax->datum tail)
                         this-syntax
                         this-syntax)]
         ...))]))

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
     (in-r-lua
       (syntax/loc this-syntax
         (#%begin body ...)))]

    [(let ~! _ ...)
     (syntax-parser/scope
      scope
      [(_ ([name (~cata val r-lua/expr)] ...)
          body ...)

       (for ([name (in-list (attribute name))])
         (bind! name (r-lua-var)))

       (in-r-lua
        (with-syntax ([(body ...) (map r-lua/expr (attribute body))])
          (syntax/loc this-syntax
            (#%let ([name val] ...)
                   (#%begin body ...)))))])]

    [(letrec ~! _ ...)
     (syntax-parser/scope
      scope
      [(_ ([name val] ...)
          body ...)

       (for ([name (in-list (attribute name))])
         (bind! name (r-lua-var)))

       (in-r-lua
        (with-syntax ([(val ...)  (map r-lua/expr (attribute val))]
                      [(body ...) (map r-lua/expr (attribute body))])
          (syntax/loc this-syntax
            (#%letrec ([name val] ...)
                      (#%begin body ...)))))])]

    [(set! ~! (~cata name check-var)
           (~cata val r-lua/expr))

     (in-r-lua
      (syntax/loc this-syntax
        (#%set! name val)))]

    [(λ ~! _ ...)
     (syntax-parser/scope
      scope
      [(_ ((~cata arg (bind! _ (r-lua-var))) ...)
          (~cata body r-lua/expr) ...)

       (in-r-lua
        (syntax/loc this-syntax
          (#%abs (arg ...)
                 (#%begin body ...))))])]

    [(if ~! (~cata cond r-lua/expr)
         (~cata then r-lua/expr)
         (~cata else r-lua/expr))

     (in-r-lua
      (syntax/loc this-syntax
        (#%if cond then else)))]

    [(#%intrinsic ~! form)

     (in-r-lua
      (with-syntax ([form (in-s-lua #'form)])
        (syntax/loc this-syntax
          (#%intrinsic form))))]

    [(ident:id arg ...)
     #:do [(define mac (lookup-in-module #'ident r-lua-macro?))]
     #:when mac
     #:cut
     (r-lua/expr
      (apply-as-transformer (r-lua-macro-trx mac)
                            #'ident
                            'expression
                            this-syntax))]

    [((~cata expr r-lua/expr) ...+)
     #:cut
     (in-r-lua
       (syntax/loc this-syntax
         (#%app expr ...)))]))
