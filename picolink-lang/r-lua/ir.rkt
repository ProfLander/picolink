#lang racket/base

(require (for-syntax racket/base
                     ee-lib
                     picolink/syntax)

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
   #%let
   #%letrec
   #%declare
   #%set!
   #%abs
   #%app
   #%if
   #%intrinsic
   #%void])

(define-for-syntax in-r-lua (in-space 'r-lua))
(define-for-syntax not-in-r-lua (not-in-space 'r-lua))
