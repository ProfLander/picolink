#lang racket/base

; IR definition utilities

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse))

(provide (all-defined-out))

(define-syntax define-ir-forms
  (syntax-parser
    [(_ name:id literal:id ...)
     (syntax/loc this-syntax
       (begin
         (define-syntax (literal stx)
           (raise-syntax-error 'literal
                               (format "may not be used outside ~a" 'name)
                               stx))
         ...))]))

(define-syntax define-ir
  (syntax-parser
    [(_ name literal:id ...)

     (with-syntax ([name-ir-literals (format-id #'name
                                                "~a-ir-literals"
                                                #'name)]
                   [name-ir-literal (format-id #'name
                                               "~a-ir-literal"
                                               #'name)])

       (syntax/loc this-syntax
         (begin
           (define-ir-forms name
             literal ...)

           (begin-for-syntax
             (define-literal-set name-ir-literals
               #:datum-literals [literal ...]
               ())

             (define-syntax-class name-ir-literal
               (pattern (~datum literal))
               ...)))))]))
