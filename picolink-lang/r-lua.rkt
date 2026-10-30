#lang picolink/language

(require racket/base
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

  (define ident
    (syntax-local-introduce (datum->syntax #f 'r-lua/prelude)))

  (require! ident ident #:space r-lua-space))
