#lang racket/base

(require (for-syntax racket/base
                     racket/path
                     syntax/parse)
         picolink/config/parameters)

(provide (all-from-out racket/base)
         (all-from-out picolink/config/parameters)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ body ...)

     (with-syntax ([root (path-only (syntax-source this-syntax))])
       #'(#%plain-module-begin
          (current-root root)
          body ...))]))
