#lang picolink/r-lua/base

(provide (all-defined))

(define-macro if
  [(_ (~cata cond r-lua/expr)
      (~cata then r-lua/expr)
      (~cata else r-lua/expr))

   (syntax/loc this-syntax
     (#%if cond then else))])
