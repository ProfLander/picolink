#lang racket/base

(require syntax/parse
         picolink/syntax
         picolink/ee-lib
         picolink/parser
         picolink/module/scopes
         picolink/intrinsic/var
         picolink/r-lua/space
         (for-template picolink/r-lua/ir
                       picolink/s-lua/ir))

(provide (all-defined-out))

(define-literal-set r-lua-surface-literals
  #:datum-literals [begin
                     require
                     provide
                     define-macro
                     #%intrinsic]
  [])

(struct/cross-phase r-lua-var [])
(struct/cross-phase r-lua-macro [trx])

(define (supported-var? binding)
  (or (r-lua-var? binding)
      (intrinsic-var? binding)))

(define (check-var ident)

  (define binding (lookup-in-module ident #:space r-lua-space))

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

    [(_:r-lua-ir-literal _ ...)
     this-syntax]

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

    [(require ~! (~var _ (bind-require this-syntax #:space r-lua-space)))
     (void)]

    [(provide ~! (~var _ (bind-provide this-syntax #:space r-lua-space)))
     (void)]

    [(define-macro ~! name expr ...)
     (bind-in-module! #'name
                      (r-lua-macro
                       (eval-transformer
                        (r-lua/macro
                         (datum->syntax this-syntax
                                        (attribute expr)
                                        this-syntax))))
                      #:space r-lua-space)
     (void)]

    [(ident:id arg ...)
     #:do [(define mac (lookup-in-module #'ident r-lua-macro?
                                         #:space r-lua-space))]
     #:when mac
     #:cut
     (r-lua/top-level
      (apply-as-transformer (r-lua-macro-trx mac)
                            #f ;#'ident
                            'definition
                            this-syntax))]

    [(~cata e r-lua/expr)
     (syntax/loc this-syntax
       e)]))

(define-splicing-syntax-class parse-option
  (pattern kw:keyword)
  (pattern (~seq kw:keyword expr)))

(define r-lua/macro
  (syntax-parser
    #:literal-sets [r-lua-surface-literals]

    [(opt:parse-option ...
      [pat body ... tail] ...)
     (syntax/loc this-syntax
       (syntax-parser
         opt ...
         [pat
          body ...
          (in-r-lua
           (datum->syntax this-syntax
                          (syntax->datum tail)
                          this-syntax
                          this-syntax))]
         ...))]))

(define/hygienic (r-lua/expr stx)
  #:expression

  (syntax-parse stx
    #:literal-sets [r-lua-surface-literals]

    [(_:r-lua-ir-literal _ ...)
     this-syntax]

    [(ident:id arg ...)
     #:do [(define mac (lookup-in-module #'ident r-lua-macro?
                                         #:space r-lua-space))]
     #:when mac
     #:cut
     (r-lua/expr
      (apply-as-transformer (r-lua-macro-trx mac)
                            #f ;#'ident
                            'expression
                            this-syntax))]

    [(~cata ident:id check-var)
     (syntax/loc this-syntax
       ident)]

    [(~or d:boolean
          d:number
          d:string)
     (syntax/loc this-syntax
       d)]

    [(#%intrinsic ~! form)

     (in-r-lua
      (with-syntax ([form (in-s-lua #'form)])
        (syntax/loc this-syntax
          (#%intrinsic form))))]

    [((~cata expr r-lua/expr) ...+)
     #:cut
     (in-r-lua
       (syntax/loc this-syntax
         (#%app expr ...)))]))
