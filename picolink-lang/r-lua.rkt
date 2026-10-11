#lang picolink/language

(require racket/base
         racket/function
         racket/match
         racket/sequence

         (except-in syntax/parse expr)

         picolink/r-lua/surface
         picolink/r-lua/s-lua)

(provide (all-defined-out))

#:extract  r-lua-extract
#:wrap     r-lua-wrap
#:parse    r-lua-parse
#:compiler r-lua-compiler

(define r-lua-extract
  (syntax-parser
    #:datum-literals [begin]
    [(begin body ...)
     (for/fold ([acc null])
               ([body (in-syntax #'(body ...))])
       (syntax-parse body
         #:datum-literals [require in]

         [(require (in mod _ ...) ...)
          (append acc (attribute mod))]

         [_ acc]))]))

(define (r-lua-wrap stx)
  #`(begin #,@stx))

(define/hygienic (r-lua-parse stx)
  #:definition
  (r-lua/top-level stx))

(define r-lua-compiler 
  (match-λ
   ['s-lua r-lua->s-lua]
   [target (error "unsupported backend" target)]))
