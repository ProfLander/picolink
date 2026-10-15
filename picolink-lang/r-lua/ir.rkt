#lang racket/base

(require (for-syntax racket/base
                     ee-lib)

         (for-meta 2
                   racket/base
                   racket/function
                   syntax/parse)

         ee-lib/define)

(provide (for-space r-lua (all-defined-out))
         (for-syntax (all-defined-out)))

(define-literal-forms r-lua-ir-literals
  #:binding-space r-lua
  "cannot use outside of r-lua"
  [#%begin
   #%define
   #%define-macro
   #%let
   #%letrec
   #%declare
   #%set!
   #%abs
   #%app
   #%if
   #%void])

(define-for-syntax in-r-lua (in-space 'r-lua))
