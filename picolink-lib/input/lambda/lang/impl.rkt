#lang racket/base

(require (for-syntax (except-in racket/base
                                compile)

                     syntax/parse

                     picolink/input/module)

         picolink/input
         picolink/input/lambda/interface)

(provide (all-from-out racket/base)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ body ...)
     (make-input-module
      this-syntax
      #'(make-input 'lambda
                    #'(body ...)))]))
