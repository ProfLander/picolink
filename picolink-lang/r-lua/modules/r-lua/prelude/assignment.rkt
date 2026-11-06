#lang picolink/r-lua/base

(provide (all-defined))

(define-macro set!
  [(_ (~cata name check-var) (~cata val r-lua/expr))
   (syntax/loc this-syntax
     (#%set! name val))])
