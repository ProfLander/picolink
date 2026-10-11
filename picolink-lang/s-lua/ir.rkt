#lang racket/base

(require ee-lib/define)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-literal-forms s-lua-ir-literals
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
   #%length])
