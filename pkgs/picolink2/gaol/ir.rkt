#lang racket/base

(require picolink2/compiler/ir)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-ir gaol
  #%datum
  #%begin
  #%define-values
  #%let-values
  #%letrec-values
  #%lambda
  #%app
  #%if
  #%set!)
