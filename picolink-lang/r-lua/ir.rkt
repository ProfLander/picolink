#lang racket/base

(require ee-lib/define)

(provide (all-defined-out))

(define-literal-forms r-lua-ir-literals
  "cannot use outside of r-lua"
  [#%begin
   #%define
   #%define-syntax
   #%let
   #%set!
   #%abs
   #%app
   #%if])

