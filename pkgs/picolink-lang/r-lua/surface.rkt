#lang racket/base

(require racket/syntax

         syntax/parse
         syntax/strip-context

         picolink/scope
         picolink/syntax
         picolink/binding
         picolink/module/compiled
         picolink/module/parameters
         picolink/program/scope
         picolink/intrinsic/var

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
                (let ([lexical (lookup ident)])
                  (if lexical
                      (values ident lexical)

                      (let ([pair (lookup-foreign ident)])

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
  (or (let ([binding (lookup ident)])

        (and (macro? binding)
             binding))

      (let* ([pair (lookup-foreign ident)]
             [binding (and pair (cdr pair))])

        (and (macro? binding)
             binding))))

(define (apply-macro mac parser stx)

  (let* ([module-ident (macro-module-ident mac)])

    (parameterize
        ([current-foreign-scopes
          (if (datum=? module-ident (current-module-ident))

              (current-foreign-scopes)

              (let* ([mac-module (with-scope (current-program-scope)
                                   (lookup module-ident))]

                     [module-scope (picolink-module-lexical-scope
                                    mac-module)])

                (cons (cons module-ident module-scope)
                      (current-foreign-scopes))))])

      (parser ((syntax-local-eval (macro-trx mac)) stx)))))

(define r-lua/top-level
  (syntax-parser
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

    [(require ~! (~var _ (bind-require this-syntax)))
     (void)]

    [(provide ~! (~var _ (bind-provide this-syntax)))
     (void)]

    [(define-macro ~! name expr ...)
     (void
      (bind! #'name
             (macro
              (r-lua/macro
               (datum->syntax this-syntax
                              (attribute expr)
                              this-syntax
                              this-syntax))
              (current-module-ident))))]

    [(ident:id _ ...)
     #:do [(define mac (lookup-macro #'ident))]
     #:when mac
     #:cut
     (apply-macro mac r-lua/top-level this-syntax)]

    [(~cata e r-lua/expr)
     (syntax/loc this-syntax
       e)]))

(define-splicing-syntax-class parse-option
  (pattern kw:keyword)
  (pattern (~seq kw:keyword expr)))

(define r-lua/macro
  (syntax-parser
    #:literal-sets [r-lua-surface-literals]

    [(opt:parse-option ... [pat body ... tail] ...)
     (syntax/loc this-syntax
       (syntax-parser
         opt ...
         [pat
          body ...
          (in-r-lua
           (replace-context this-syntax tail))]
         ...))]))

(define (r-lua/expr stx)
  (with-new-scope expr
    (syntax-parse stx
      #:literal-sets [r-lua-surface-literals]

      [(_:r-lua-ir-literal _ ...)
       this-syntax]

      [(ident:id _ ...)
       #:do [(define mac (lookup-macro #'ident))]
       #:when mac
       #:cut

       (apply-macro mac r-lua/expr this-syntax)]

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
          (#%app expr ...)))])))
