#lang picolink/r-lua/base

(require r-lua/prelude/definition
         r-lua/prelude/lambda)

(provide (all-defined))

(define (identity x)
  x)

(define (const t)
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return t)))))

(define-macro thunk
  [(_ body ...)
   (syntax/loc this-syntax
     (λ ()
       body ...))])
