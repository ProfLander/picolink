#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     picolink/lang/interaction
                     picolink/lang/begin))

(provide #%module-begin
         #%top-interaction)

(define-syntax #%module-begin
  (syntax-parser
    [(_ language body ...)
     #'(#%program-begin language body ...)]))

(define-syntax #%top-interaction
  (syntax-parser
    [(_ . form)
     (#%program-interaction this-syntax #'form)]))
