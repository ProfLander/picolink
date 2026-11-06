#lang racket/base

(require syntax/parse
         syntax/strip-context
         picolink/syntax
         picolink/ee-lib
         picolink/binding
         picolink/module/parsed
         picolink/module/scopes
         picolink/intrinsic/var
         picolink/r-lua/space
         (for-template picolink/r-lua/ir))

(provide (all-defined-out))

(define-literal-set r-lua-surface-literals
  #:datum-literals [begin
                     require
                     provide
                     define-macro]
  [])

(struct/cross-phase r-lua-var [])

(define (supported-var? binding)
  (or (r-lua-var? binding)
      (intrinsic-var? binding)))

(define (check-var ident)

  (let-values ([(ident binding)
                (let ([lexical (lookup ident #:space r-lua-space)])
                  (if lexical
                      (values ident lexical)

                      (let ([pair (lookup-in-modules
                                   ident
                                   #:space r-lua-space)])

                        (if pair
                            (values (syntax-property ident
                                                     'bound-from
                                                     (car pair)
                                                     #t)
                                    (cdr pair))

                            (values ident #f)))))])

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

    ident))

(define (lookup-macro ident)
  (or (lookup ident macro? #:space r-lua-space)
      (let ([pair (lookup-in-modules ident macro? #:space r-lua-space)])
        (and pair (cdr pair)))))

(define (apply-macro mac ctx-type parser stx)

  (let ([module-ident (macro-module-ident mac)]
        [module-scope (macro-module-scope mac)])

    (parameterize ([current-module-scopes
                    (cons (cons module-ident module-scope)
                          (current-module-scopes))])

      (parser
       (let ([scope (current-def-ctx)])

         (apply-as-transformer (λ (stx)
                                 (parameterize ([current-def-ctx scope])
                                   ((macro-trx mac) stx)))
                               #f
                               ctx-type
                               stx))))))

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
     (void
      (bind! #'name
             (macro
              (eval-transformer
               (r-lua/macro
                (datum->syntax this-syntax
                               (attribute expr)
                               this-syntax
                               this-syntax)))
              (current-module-ident)
              (current-def-ctx))
             #:space r-lua-space))]

    [(ident:id _ ...)
     #:do [(define mac (lookup-macro #'ident))]
     #:when mac
     #:cut
     (apply-macro mac 'definition r-lua/top-level this-syntax)]

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
           (replace-context this-syntax tail))]
         ...))]))

(define/hygienic (r-lua/expr stx)
  #:expression

  (syntax-parse stx
    #:literal-sets [r-lua-surface-literals]

    [(_:r-lua-ir-literal _ ...)
     this-syntax]

    [(ident:id _ ...)
     #:do [(define mac (lookup-macro #'ident))]
     #:when mac
     #:cut

     (apply-macro mac 'definition r-lua/expr this-syntax)]

    [(~cata ident:id check-var)
     #'ident]

    [(~or d:boolean
          d:number
          d:string)
     this-syntax]

    [((~cata expr r-lua/expr) ...+)
     #:cut
     (in-r-lua
       (syntax/loc this-syntax
         (#%app expr ...)))]))
