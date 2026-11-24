#lang racket/base

(require picolink2/compiler/ir)

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-ir s-lua

  #%datum

  #%chunk
  #%block

  #%assign
  #%local
  #%label
  #%goto
  #%do

  #%for
  #%in
  #%while
  #%repeat
  #%until
  #%break

  #%if
  #%then
  #%elseif
  #%else

  #%function
  #%vararg
  #%return
  #%call

  #%table
  #%member
  #%method

  #%prefix

  #%nil

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
  #%length)
