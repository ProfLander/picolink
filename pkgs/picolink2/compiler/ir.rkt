#lang racket/base

; IR definition utilities

(require (for-syntax racket/base
                     racket/function
                     racket/syntax
                     syntax/parse
                     racket/require-transform
                     racket/provide-transform))

(provide (all-defined-out))

(define-for-syntax (in-space space stx)
  (let ([intro (make-interned-syntax-introducer space)])
    (intro stx 'add)))

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

                   [define-name-form (format-id #'name
                                                  "define-~a-form"
                                                  #'name)]

                   [define-name-forms (format-id #'name
                                                 "define-~a-forms"
                                                 #'name)]

                   [define-name-pass-form (format-id #'name
                                                     "define-~a-pass-form"
                                                     #'name)]

                   [define-name-pass (format-id #'name
                                                "define-~a-pass"
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
               (curry in-space 'name))

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
               [(_ ident transformer)
                (with-syntax ([ident* (in-name #'ident)])
                  (syntax/loc this-syntax
                    (define-syntax ident* transformer)))]

               [(_ (ident arg (... ...)) body (... ...))
                (with-syntax ([ident* (in-name #'ident)])
                  (syntax/loc this-syntax
                    (define-syntax (ident* arg (... ...))
                      body (... ...))))]))

           (define-syntax define-name-pass-form
             (syntax-parser
               #:datum-literals [->]
               [(_ (ident -> space)
                   [pat
                    body (... ...)
                    tail]
                   (... ...))

                (syntax/loc this-syntax
                  (define-name-syntax ident
                    (syntax-parser
                      [pat body (... ...) (in-space 'space tail)]
                      (... ...))))]))

           (define-syntax define-name-pass
             (syntax-parser
               [(_ space
                   (~and %clause
                         (~or
                          (~and [(ident pat (... ...) . pat-tail)
                                 form (... ...)]
                                (~parse clause
                                        (syntax/loc #'%clause
                                          [(_ pat (... ...) . pat-tail)
                                           form (... ...)])))

                          (~and [(ident pat (... ...))
                                 form (... ...)]
                                (~parse clause (syntax/loc #'%clause
                                                 [(_ pat (... ...))
                                                  form (... ...)])))))
                   (... ...))

                (syntax/loc this-syntax
                  (begin
                    (define-name-pass-form (ident -> space)
                      clause)
                    (... ...)))]))

           (define-syntax define-name-form
             (syntax-parser
               [(_ ident clause (... ...))

                (syntax/loc this-syntax
                  (define-name-pass-form (ident -> name)
                    clause (... ...)))]))

           (define-syntax define-name-forms
             (syntax-parser
               [(_ (~and %clause
                         (~or
                          (~and [(ident pat (... ...) . pat-tail)
                                 form (... ...)]
                                (~parse clause
                                        (syntax/loc #'%clause
                                          [(_ pat (... ...) . pat-tail)
                                           form (... ...)])))

                          (~and [(ident pat (... ...))
                                 form (... ...)]
                                (~parse clause (syntax/loc #'%clause
                                                 [(_ pat (... ...))
                                                  form (... ...)])))))
                   (... ...))
                (syntax/loc this-syntax
                  (begin
                    (define-name-form ident
                      clause)
                    (... ...)))]))

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
