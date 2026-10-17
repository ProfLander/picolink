#lang picolink/language

(require (for-syntax racket/base
                     syntax/parse)

         racket/match
         racket/sequence
         syntax/parse
         picolink/syntax
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

         [(require (only-in mod _ ...) ...)
          (append acc (attribute mod))]

         [_ acc]))]))

(define wrap-begin
  (syntax-parser
    [(body ...)
     (syntax/loc this-syntax
       (begin body ...))]))

(define require?
  (syntax-parser
    #:literal-sets [dsl-surface-literals]
    [(require _ ...) #t]
    [_ #f]))

(define provide?
  (syntax-parser
    #:literal-sets [dsl-surface-literals]
    [(provide _ ...) #t]
    [_ #f]))

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

    [(require (only-in mod
                       (~or (~and [req-from req-to]
                                  (~do (require! this-syntax
                                                 #'mod
                                                 #'req-to
                                                 (syntax-e #'req-from))))
                            (~and req
                                  (~do (require! this-syntax
                                                 #'mod
                                                 #'req))))
                  ...)
              ...)
     (void)]

    [(provide (~or (~and [prov-from prov-to]
                         (~do (provide! this-syntax
                                        #'prov-from
                                        #'prov-to)))
                   (~and prov
                         (~do (provide! this-syntax
                                        #'prov))))
              ...)
     (void)]

    [(define ident (~cata expr (dsl/expr _)))
     (bind! #'ident (dsl-var))
     #'(#%define ident expr)]

    [(define-macro ident expr)
     (bind! #'ident (dsl-syntax (eval-transformer (dsl/macro #'expr))))
     #'(#%define-macro ident expr)]

    [(~cata e dsl/expr)
     #'e]))

(define/hygienic-expression-parser dsl/expr
  #:literal-sets [dsl-surface-literals]

  [n:number #'n]

  [ident:id

   (unless (lookup #'ident dsl-var?)
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
   #:do [(define mac (lookup #'ident dsl-syntax?))]
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
