#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/lang/compiler
         picolink/lang/input)

(provide (all-defined-out))

(define-syntax #%program-begin
  (syntax-parser
    [(_ compiling language body ...)

     (if #t #;(syntax-e #'compiling)

         (#%input-begin this-syntax
                        #'language
                        (attribute body))

         (#%compiler-begin this-syntax
                           #'language
                           (attribute body)))]))
