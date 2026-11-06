#lang picolink/r-lua/base

(provide (all-defined))

(define-macro begin
  [(_ body ...)
   (with-syntax ([(body ...) (map r-lua/expr (attribute body))])
     (syntax/loc this-syntax
       (#%begin body ...)))])
