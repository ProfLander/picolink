#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/lambda      λ))

(provide (all-defined))

(define (identity x)
  x)

(define (const t)
  (λ [: vs values]
    t))

(define-macro thunk
  [(_ body ...)
   (syntax/loc this-syntax
     (λ ()
       body ...))])
