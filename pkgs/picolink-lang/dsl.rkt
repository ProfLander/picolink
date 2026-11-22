#lang picolink/language

(require (for-syntax racket/base
                     syntax/parse)

         racket/match
         racket/sequence
         racket/syntax

         syntax/parse

         picolink/scope
         picolink/syntax
         picolink/dsl/racket
         picolink/dsl/surface

         (for-template picolink/dsl/ir))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

#:extract  extract-dependencies
#:wrap     wrap-begin
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

         [(require reqs:extract-require)
          (append acc (attribute reqs.dependencies))]

         [_ acc]))]))

(define wrap-begin
  (syntax-parser
    [(body ...)
     (syntax/loc this-syntax
       (begin body ...))]))

(define (dsl/top-level stx)
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
     (bind! #'ident (dsl-var))
     #'(#%define ident expr)]

    [(define-macro ident expr)
     (bind! #'ident
            (dsl-syntax (syntax-local-eval (dsl/macro #'expr))))
     #'(#%define-macro ident expr)]

    [(~cata e dsl/expr)
     #'e]))

(define (dsl/expr stx)
  (with-new-scope expr
    (syntax-parse stx
      #:literal-sets [dsl-surface-literals]

      [n:number #'n]

      [ident:id

       (unless (dsl-var? (lookup #'ident))
         (parameterize ([error-print-context-length 0])
           (raise-syntax-error #f
                               "unbound variable"
                               #'ident)))

       #'ident]

      [(let _ ...)
       (with-new-scope let
         (syntax-parse this-syntax
           [(_ ([(~cata ident (bind! _ (dsl-var)))
                 (~cata expr (dsl/expr _))])
               (~cata body dsl/expr) ...)
            #'(#%let ([ident expr]) body ...)]))]

      [(ident arg ...)
       #:do [(define mac (dsl-syntax? (lookup #'ident)))]
       #:when mac
       (dsl/expr
        (syntax-local-apply-transformer
         (dsl-syntax-trx mac)
         #'ident
         'expression
         #f
         #'(ident arg ...)))])))

(define (dsl/macro stx)
  (with-new-scope macro
    (syntax-parse stx
      #:literal-sets [dsl-surface-literals]

      [(pat body ...)
       #'(syntax-parser
           [pat body ...])])))

(define dsl-compiler
  (match-λ
   ['racket (compose dsl-compile/racket dsl/top-level)]
   [target (error "unsupported backend" target)]))
