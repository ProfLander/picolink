#lang racket/base

(require (for-syntax (except-in racket/base
                                compile)

                     (except-in syntax/parse
                                expr))

         picolink/input
         picolink/lambda)

(provide (all-from-out racket/base)
         (all-from-out picolink/lambda)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(head body ...)
     (make-input-module
      this-syntax
      (with-syntax ([print (datum->syntax this-syntax 'print)])
        #'(make-lambda
           (lambda/check-binds
            (#%intrinsic print)
            body ...))))]))
