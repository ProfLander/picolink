#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     picolink/lang/expand-state)

         picolink/lang/entry-point
         picolink/lang/dependency)

(provide (all-defined-out))

(define-syntax #%program-begin
  (syntax-parser
    [(_ language body ...)

     (let ([is-entry-point (and (not (syntax-local-compiling-module?))
                                (current-is-entry-point))])

       (if is-entry-point

           (#%entry-point-begin this-syntax
                                #'language
                                (attribute body))

           (#%dependency-begin this-syntax
                               #'language
                               (attribute body))))]))
