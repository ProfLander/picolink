#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         picolink/syntax)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-literal-forms dsl-ir-literals
  "cannot use outside of dsl"
  [#%begin
   #%define
   #%define-macro
   #%let])
