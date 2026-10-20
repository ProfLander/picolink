#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/sequence

         syntax/parse

         ee-lib
         ee-lib/define)

(provide (all-from-out ee-lib
                       ee-lib/define)
         (all-defined-out))

(define-syntax define/hygienic-syntax-parse
  (syntax-parser
    [(_ (name stx arg ...) ctx-type clause ...)
     #'(define/hygienic (name stx arg ...)
         ctx-type
         (syntax-parse stx
           clause ...))]))

(define-syntax define/hygienic-syntax-parser
  (syntax-parser
    [(_ name ctx-type clause ...)
     #'(define/hygienic (name arg)
         ctx-type
         (syntax-parse arg
           clause ...))]))

(define-syntax define/hygienic-definition-parser
  (syntax-parser
    [(_ name body ...)
     #'(define/hygienic-syntax-parser name
         #:definition
         body ...)]))

(define-syntax define/hygienic-expression-parser
  (syntax-parser
    [(_ name body ...)
     #'(define/hygienic-syntax-parser name
         #:definition
         body ...)]))

(define-syntax syntax-parser/scope
  (syntax-parser
    [(_ scope clause ...)
     (with-syntax ([stx (datum->syntax #'scope
                                       'this-syntax
                                       #'scope
                                       #'scope)])
       #'(with-scope scope
           (syntax-parse stx
             clause ...)))]))

(define scope-idents internal-definition-context-binding-identifiers)

(define (in-scope scope
                  [predicate (lambda (_) #t)]
                  #:space [space #f])

  (sequence-map (λ (id)
                  (values id
                          (parameterize ([current-def-ctx scope])
                            (lookup id predicate #:space space))))

                (in-list (scope-idents scope))))
