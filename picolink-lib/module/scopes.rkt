#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/ee-lib)

(provide (all-defined-out))

(define current-require-scope
  (make-parameter #f))

(define current-module-scope
  (make-parameter #f))

(define current-provide-scope
  (make-parameter #f))

(define-syntax with-module-scopes
  (syntax-parser
    [(_ body ...)
     #'(with-scope require-scope
         (with-scope module-scope
           (with-scope provide-scope
             (parameterize ([current-require-scope require-scope]
                            [current-module-scope  module-scope]
                            [current-provide-scope provide-scope])
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

(define (lookup-in-module ident
                          [predicate (lambda (v) #t)]
                          #:space [binding-space #f])

  "Lookup IDENT in the current module scope"

  (define res
    (lookup-in (current-module-scope)
               ident
               predicate
               #:space binding-space))

  res)

(define (bind-in-require! ident val)
  (bind-in! (current-require-scope) ident val))

(define (bind-in-provide! ident val)
  (bind-in! (current-provide-scope) ident val))

(define (bind-in-module! ident val)
  (bind-in! (current-module-scope) ident val))
