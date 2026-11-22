#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/syntax
         picolink/scope
         picolink/module/parameters)

(provide (all-defined-out))

(struct/cross-phase picolink-module [ident
                                     program-scope
                                     require-scope
                                     provide-scope
                                     lexical-scope
                                     stx])

(define-syntax with-picolink-module-scopes
  (syntax-parser
    [(_ parsed body ...)
     (syntax/loc this-syntax

       (parameterize ([current-require-scope
                       (picolink-module-require-scope parsed)]
                      [current-provide-scope
                       (picolink-module-provide-scope parsed)]
                      [current-foreign-scopes null])

         (with-scope (picolink-module-lexical-scope parsed)
           body ...)))]))
