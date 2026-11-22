#lang picolink/language

(require racket/base
         racket/match
         racket/sequence

         (except-in syntax/parse expr)

         picolink/r-lua/surface
         picolink/r-lua/s-lua)

(provide (all-defined-out))

#:search-paths r-lua-search-paths
#:extract      r-lua-extract
#:wrap         r-lua-wrap
#:compiler     r-lua-compiler

(define (r-lua-search-paths)
  (list 'picolink/r-lua/modules))

(define r-lua-extract
  (syntax-parser
    #:literal-sets [r-lua-surface-literals]
    [(begin body ...)
     (for/fold ([acc null])
               ([body (in-syntax #'(body ...))])
       (syntax-parse body
         #:literal-sets [r-lua-surface-literals]

         [(require reqs:extract-require)
          (append acc (attribute reqs.dependencies))]

         [_ acc]))]))

(define (r-lua-wrap stx)
  #`(begin #,@stx))

(define r-lua-compiler 
  (match-λ
   ['s-lua (compose r-lua->s-lua r-lua/top-level)]
   [target (error "unsupported backend" target)]))
