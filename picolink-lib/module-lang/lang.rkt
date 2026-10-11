#lang racket/base

(require (for-syntax racket/base
                     racket/syntax

                     syntax/parse

                     picolink/syntax
                     picolink/lang/interaction)

         picolink/lang/begin)

(provide (all-from-out racket/base)
         #%module-begin)

(define-syntax (for-module stx)
  (raise-syntax-error #f
                      "cannot be used outside of module-lang"
                      stx))

(define-syntax #%module-begin
  (syntax-parser
    [(_ language mod-body ...)

     #'(#%plain-module-begin

        (provide #%module-begin
                 #%top-interaction
                 for-module)

        mod-body ...

        (define-syntax #%module-begin
          (syntax-parser
            #:literals [for-module]
            [(_ (~or (~optional (for-module fm (... ...))
                                #:defaults ([[fm 1] null]))
                     body)
                (... ...))

             (with-syntax* ([language (datum->syntax this-syntax
                                                     'language)]
                            [cont
                             (forward-macro
                              this-syntax
                              #'(#%program-begin language
                                                 body (... ...)))])

               #'(#%plain-module-begin
                  fm (... ...)
                  cont))]))

        (define-syntax #%top-interaction
          (syntax-parser
            [(_ . form)
             (#%program-interaction this-syntax #'form)])))]))
