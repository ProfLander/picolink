#lang picolink/language

(require racket/base
         racket/function
         racket/match

         (prefix-in s-lua: picolink/s-lua/lua)
         (for-template picolink/s-lua/ir))

(provide (all-defined-out))

#:extract  s-lua-extract
#:wrap     s-lua-wrap
#:parse    s-lua-parse
#:compiler s-lua-compiler

(define s-lua-extract (const null))

(define (s-lua-wrap stx)
  #`(#%chunk (#%block #,@stx)))

(define s-lua-parse identity)

(define s-lua-compiler 
  (match-λ
   ['s-lua identity]
   ['lua s-lua:compile/chunk]
   [target (error "unsupported backend" target)]))
