#lang racket/base

(require ee-lib/define)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-literal-forms dsl-ir-literals
  "cannot use outside of dsl"
  [#%begin
   #%define
   #%define-macro
   #%let])
