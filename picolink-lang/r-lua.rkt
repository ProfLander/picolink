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
#:inject   r-lua-inject
#:parse    r-lua-parse
#:compiler r-lua-compiler

(define r-lua-extract
  (syntax-parser
    #:literal-sets [r-lua-surface-literals]
    [(begin body ...)
     (for/fold ([acc null])
               ([body (in-syntax #'(body ...))])
       (syntax-parse body
         #:literal-sets [r-lua-surface-literals]

         [(require (~var reqs (extract-require this-syntax)))
          (append acc (attribute reqs.dependencies))]

         [_ acc]))]))

(define (r-lua-wrap stx)
  #`(begin #,@stx))

(define (r-lua-inject)
  (define ident (syntax-local-introduce (datum->syntax #f 'lua)))
  (require! ident ident))

(define/hygienic (r-lua-parse stx)
  #:definition
  (r-lua/top-level stx))

(define r-lua-compiler 
  (match-λ
   ['s-lua r-lua->s-lua]
   [target (error "unsupported backend" target)]))
