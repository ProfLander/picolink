#lang picolink/r-lua/base

(require r-lua/prelude/lambda)

(provide (all-defined))

(define-macro define

  [(_ (name:id arg ...) body ...)
   (syntax/loc this-syntax
     (define name (λ (arg ...) body ...)))]

  [(_ (name:id arg ... . tail) body ...)
   (syntax/loc this-syntax
     (define name (λ (arg ... . tail) body ...)))]

  [(_ (name:id . tail) body ...)
   (syntax/loc this-syntax
     (define name (λ tail body ...)))]

  [(_ name:id expr)

   (bind! #'name (r-lua-var) #:space r-lua-space)

   (with-syntax ([expr (r-lua/expr #'expr)])
     (syntax/loc this-syntax
       (#%define name expr)))])
