#lang picolink/language

(require racket/base
         picolink/backend
         picolink/r-lua/base
         picolink/r-lua/space)

(provide (all-defined-out))

#:search-paths r-lua-search-paths
#:modules      r-lua-modules
#:extract      r-lua-extract
#:wrap         r-lua-wrap
#:inject       r-lua-inject
#:parse        r-lua-parse
#:compiler     r-lua-compiler

(define (r-lua-modules)
  (list #'r-lua/prelude))

(define (r-lua-inject)
  (let ([r-lua-prelude
         (syntax-local-introduce (datum->syntax #f 'r-lua/prelude))])
    (require! r-lua-prelude r-lua-prelude #:space r-lua-space)))
