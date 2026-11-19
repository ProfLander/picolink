#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/scope
         picolink/syntax)

(provide (all-defined-out))

(define current-require-scope
  (make-parameter #f))

(define current-provide-scope
  (make-parameter #f))

(define current-foreign-scopes
  (make-parameter null))

(define-syntax with-module-scopes
  (syntax-parser
    [(_ body ...)
     (syntax/loc this-syntax

       (parameterize ([current-require-scope (make-scope 'require)]
                      [current-provide-scope (make-scope 'provide)]
                      [current-foreign-scopes null])

         (with-new-scope definition
           body ...)))]))

(define (lookup-in-require ident)

  "Lookup IDENT in the current require scope"

  (with-scope (current-require-scope)
    (lookup ident)))

(define (lookup-foreign ident)

  "Lookup IDENT in the current module scope"

  (for/first ([pair (in-list (current-foreign-scopes))]
              #:do [(define source-module (car pair))
                    (define scope (cdr pair))

                    (define bind
                      (with-scope scope
                        (lookup ident)))]

              #:when bind)

    (cons source-module bind)))
