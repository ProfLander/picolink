#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/s-lua       s-lua))

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
