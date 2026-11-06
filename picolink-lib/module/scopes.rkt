#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/list

         picolink/syntax
         picolink/ee-lib)

(provide (all-defined-out))

(define current-require-scope
  (make-parameter #f))

(define current-module-scopes
  (make-parameter null))

(define current-provide-scope
  (make-parameter #f))

(define-syntax with-module-scopes
  (syntax-parser
    [(_ body ...)
     #'(with-scope require-scope
         (with-scope provide-scope
           (with-scope module-scope
             (parameterize ([current-require-scope require-scope]
                            [current-provide-scope provide-scope]
                            [current-module-scopes null])
               body ...))))]))

(define (lookup-in-require ident
                           [predicate (lambda (v) #t)]
                           #:space [binding-space #f])

  "Lookup IDENT in the current require scope"

  (lookup-in (current-require-scope)
             ident
             predicate
             #:space binding-space))

(define (lookup-in-provide ident
                           [predicate (lambda (v) #t)]
                           #:space [binding-space #f])

  "Lookup IDENT in the current provide scope"

  (lookup-in (current-provide-scope)
             ident
             predicate
             #:space binding-space))

(define (lookup-in-modules ident
                           [predicate (lambda (v) #t)]
                           #:space [binding-space #f])

  "Lookup IDENT in the current module scope"

  (for/first ([i (in-naturals)]
              [pair (in-list (current-module-scopes))]
              #:do [(define source-module (car pair))
                    (define scope (cdr pair))

                    (define idents (scope-idents scope))

                    (define scope-tail (member ident idents datum=?))
                    (define scope-ident (and scope-tail
                                             (car scope-tail)))

                    (define bind
                      (and scope-ident
                           (lookup-in scope
                                      scope-ident
                                      predicate
                                      #:space binding-space)))]

              #:when bind)

    (cons source-module bind)))

(define (bind-in-require! ident val #:space [space #f])
  (bind-in! (current-require-scope) ident val #:space space))

(define (bind-in-provide! ident val #:space [space #f])
  (bind-in! (current-provide-scope) ident val #:space space))
