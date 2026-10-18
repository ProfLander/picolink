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

         [(require (~or (~and mod:id
                              (~do (set! acc
                                         (append acc
                                                 (list #'mod)))))

                        (~and (only only-mod:id _ ...)
                              (~do (set! acc
                                         (append acc
                                                 (list #'only-mod)))))

                        (~and (prefix _:id prefix-mod:id)
                              (~do (set! acc
                                         (append acc
                                                 (list #'prefix-mod)))))

                        (~and (all-except all-except-mod:id _...)
                              (~do
                               (set! acc
                                     (append acc
                                             (list #'all-except-mod)))))

                        (~and (prefix-all-except
                               _
                               prefix-all-except-mod:id
                               _ ...)
                              (~do
                               (set! acc
                                     (append acc
                                             (list
                                              #'prefix-all-except-mod)))))

                        (~and (rename rename-mod:id _ _)
                              (~do (set! acc
                                         (append acc
                                                 (list #'rename-mod))))))
                   ...)

          acc]

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

    [(require (~or (~and mod:id
                         (~do (require! this-syntax #'mod)))

                   (~and (only only-mod:id req:id ...)
                         (~do (require/only! this-syntax
                                             #'only-mod
                                             (attribute req))))

                   (~and (prefix req-prefix:id prefix-mod:id)
                         (~do (require/prefix! this-syntax
                                               #'req-prefix
                                               #'prefix-mod)))

                   (~and (all-except
                          all-except-mod:id
                          all-except-exception:id ...)

                         (~do (require/all-except!
                               this-syntax
                               #'all-except-mod
                               (attribute all-except-exception))))

                   (~and (prefix-all-except
                          all-except-prefix:id
                          prefix-all-except-mod:id
                          prefix-all-except-exception:id ...)

                         (~do (require/all-except!
                               this-syntax
                               #'all-except-prefix
                               #'prefix-all-except-mod
                               (attribute prefix-all-except-exception))))

                   (~and (rename rename-mod:id req-from:id req-to:id)
                         (~do (require/rename! this-syntax
                                               #'rename-mod
                                               #'req-from
                                               #'req-to))))
              ...)
     (void)]

    [(provide (~or (~and prov:id
                         (~do (provide! this-syntax
                                        #'prov)))

                   (~and (rename prov-from:id prov-to:id)
                         (~do (provide/rename! this-syntax
                                               #'prov-from
                                               #'prov-to)))

                   (~and (all-from mod:id)
                         (~do (provide/all-from! this-syntax #'mod)))

                   (~and (all-from-except except-mod:id
                                          from-exception:id ...)
                         (~do (provide/all-from-except!
                               this-syntax
                               #'except-mod
                               (attribute from-exception))))

                   (~and (all-defined)
                         (~do (provide/all-defined! this-syntax)))

                   (~and (all-defined-except ident:id ...)
                         (~do (provide/all-defined-except!
                               this-syntax
                               (attribute ident))))

                   (~and (prefix-all-defined prefix-id:id)
                         (~do (provide/prefix-all-defined! this-syntax
                                                           #'prefix-id)))

                   (~and (prefix-all-defined-except except-prefix:id
                                                    except-ident:id ...)
                         (~do (provide/prefix-all-defined-except!
                               this-syntax
                               #'except-prefix
                               (attribute except-ident)))))
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
