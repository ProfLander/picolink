#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         ee-lib)

(provide (all-defined-out))

(define current-require-scope
  (make-parameter #f))

(define current-module-scope
  (make-parameter #f))

(define current-provide-scope
  (make-parameter #f))

(define-syntax with-module-scopes
  (syntax-parser
    [(_ body ...)
     #'(with-scope require-scope
         (with-scope module-scope
           (with-scope provide-scope
             (parameterize ([current-require-scope require-scope]
                            [current-module-scope  module-scope]
                            [current-provide-scope provide-scope])
               body ...))))]))
