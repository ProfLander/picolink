#lang racket/base

(require (for-syntax racket/base
                     ee-lib
                     picolink/syntax)

         ee-lib/define)

(provide (for-space s-lua (all-defined-out))
         (for-syntax (all-defined-out)))

(define-literal-forms s-lua-ir-literals
  #:binding-space s-lua
  "cannot use outside of s-lua"
  [#%chunk
   #%block
   #%assign
   #%label
   #%break
   #%goto
   #%do
   #%while
   #%repeat
   #%until
   #%if
   #%for
   #%in
   #%local
   #%then
   #%elseif
   #%else
   #%function
   #%vararg
   #%return
   #%member
   #%nil
   #%call
   #%method
   #%table
   #%add
   #%sub
   #%mul
   #%div
   #%exp
   #%mod
   #%cat
   #%lt
   #%le
   #%gt
   #%ge
   #%eq
   #%ne
   #%and
   #%or
   #%neg
   #%not
   #%length
   quote])

(define-for-syntax in-s-lua (in-space 's-lua))
(define-for-syntax not-in-s-lua (not-in-space 's-lua))
