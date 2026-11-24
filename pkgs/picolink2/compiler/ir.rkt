#lang racket/base

; IR definition utilities

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/require-transform
                     racket/provide-transform))

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
                                               #'name)]

                   [in-name (format-id #'name"in-~a" #'name)]

                   [local-expand-name (format-id #'name
                                                 "local-expand-~a"
                                                 #'name)]

                   [define-name-syntax (format-id #'name
                                                  "define-~a-syntax"
                                                  #'name)]

                   [for-name (format-id #'name "for-~a" #'name)])

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
               ...)

             (define in-name
               (let ([intro (make-interned-syntax-introducer 'name)])
                 (λ (stx)
                   (intro stx 'add))))

             (define (local-expand-name stx [ctx #f])
               (local-expand stx 'expression null ctx)))

           (define-syntax name
             (syntax-parser
               [(_ body (... ...))
                (in-name
                 (syntax/loc this-syntax
                   (begin
                     body (... ...))))]))

           (define-syntax define-name-syntax
             (syntax-parser
               [(_ name transformer)
                (with-syntax ([name* (in-name #'name)])
                  (syntax/loc this-syntax
                    (define-syntax name* transformer)))]))

           (begin-for-syntax

             (define require-for-name
               (syntax-parser
                 [(_ spec (... ...))
                  (expand-import #'(for-space name spec (... ...)))]))

             (define (provide-for-name stx modes)
               (syntax-parse stx
                 [(_ spec (... ...))
                  (expand-export #'(for-space name spec (... ...))
                                 modes)]))

             (define-struct %for-name []

               #:property prop:require-transformer
               (λ (_) require-for-name)

               #:property prop:provide-transformer
               (λ (_) provide-for-name)))

           (define-syntax for-name (%for-name)))))]))
