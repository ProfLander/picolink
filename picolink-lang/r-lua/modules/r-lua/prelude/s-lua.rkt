#lang picolink/r-lua/base

(provide (all-defined))

(define-macro s-lua
  [(_ form)

   (with-syntax ([form (in-s-lua #'form)])
     (syntax/loc this-syntax
       (#%s-lua form)))])
