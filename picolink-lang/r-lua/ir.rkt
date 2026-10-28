#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
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
  [#%module-begin
   #%begin
   #%define
   #%let
   #%letrec
   #%declare
   #%set!
   #%abs
   #%app
   #%if
   #%s-lua])

(begin-for-syntax
  (define in-r-lua (in-space 'r-lua))
  (define not-in-r-lua (not-in-space 'r-lua))

  (define-syntax-class r-lua-ir-literal
    #:literal-sets [r-lua-ir-literals]
    (pattern (~or #%module-begin
                  #%begin
                  #%define
                  #%let
                  #%letrec
                  #%declare
                  #%set!
                  #%abs
                  #%app
                  #%if
                  #%s-lua))))
