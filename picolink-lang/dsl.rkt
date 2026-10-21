#lang picolink/language

(require (for-syntax racket/base
                     syntax/parse)

         racket/match
         racket/sequence

         syntax/parse

         picolink/syntax
         picolink/module/scopes
         picolink/dsl/racket
         picolink/dsl/surface

         (for-template picolink/dsl/ir))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

#:extract  extract-dependencies
#:wrap     wrap-begin
#:parse    dsl/top-level
#:compiler dsl-compiler

(struct/cross-phase dsl-var [])
(struct/cross-phase dsl-syntax [trx])

(define extract-dependencies
  (syntax-parser
    #:literal-sets [dsl-surface-literals]
    [(begin body ...)
     (for/fold ([acc null])
               ([body (in-syntax #'(body ...))])

       (syntax-parse body
         #:literal-sets [dsl-surface-literals]

         [(require (~var reqs (extract-require this-syntax)))
          (append acc (attribute reqs.dependencies))]

         [_ acc]))]))

(define wrap-begin
  (syntax-parser
    [(body ...)
     (syntax/loc this-syntax
       (begin body ...))]))

(define/hygienic (dsl/top-level stx)
  #:definition

  (syntax-parse stx
    #:literal-sets [dsl-surface-literals]

    [(begin body ...)

     (define-values (requires provides bodies)
       (lift-requires+provides require? provide? (attribute body)))

     (for ([req (in-list requires)])
       (dsl/top-level req))

     (define bodies*
       (for/list ([body (in-list bodies)])
         (dsl/top-level body)))

     (for ([prov (in-list provides)])
       (dsl/top-level prov))

     (with-syntax ([(body ...) bodies*])
       #'(#%begin body ...))]

    [(require (~var req (bind-require this-syntax)))
     (void)]

    [(provide (~var prov (bind-provide this-syntax)))
     (void)]

    [(define ident (~cata expr (dsl/expr _)))
     (bind-in-module! #'ident (dsl-var))
     #'(#%define ident expr)]

    [(define-macro ident expr)
     (bind-in-module! #'ident
                      (dsl-syntax (eval-transformer (dsl/macro #'expr))))
     #'(#%define-macro ident expr)]

    [(~cata e dsl/expr)
     #'e]))

(define/hygienic-expression-parser dsl/expr
  #:literal-sets [dsl-surface-literals]

  [n:number #'n]

  [ident:id

   (unless (lookup-in-module #'ident dsl-var?)
     (parameterize ([error-print-context-length 0])
       (raise-syntax-error #f
                           "unbound variable"
                           #'ident)))

   #'ident]

  [(let _ ...)
   (syntax-parser/scope
    scope
    [(_ ([(~cata ident (bind! _ (dsl-var)))
          (~cata expr (dsl/expr (add-scope _ scope)))])
        (~cata body dsl/expr) ...)
     #'(#%let ([ident expr]) body ...)])]

  [(ident arg ...)
   #:do [(define mac (lookup-in-module #'ident dsl-syntax?))]
   #:when mac
   (dsl/expr
    (apply-as-transformer (dsl-syntax-trx mac)
                          #'ident
                          'expression
                          #'(ident arg ...)))])

(define/hygienic-expression-parser dsl/macro
  #:literal-sets [dsl-surface-literals]

  [(pat body ...)
   #'(syntax-parser
       [pat body ...])])

(define dsl-compiler
  (match-λ
   ['racket dsl-compile/racket]
   [target (error "unsupported backend" target)]))
