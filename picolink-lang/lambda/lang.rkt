#lang racket/base

(require (for-syntax (except-in racket/base
                                compile)

                     (except-in syntax/parse
                                expr))

         picolink/language
         picolink/lambda)

(provide (all-from-out racket/base)
         (all-from-out picolink/lambda)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ body ...)
     (make-language-module
      this-syntax
      #'(make-lambda
         (lambda/check-binds
          body ...)))]))
