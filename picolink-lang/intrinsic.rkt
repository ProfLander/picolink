#lang picolink/language

(require racket/base
         racket/function
         
         syntax/parse

         picolink/intrinsic/var)

(provide (all-defined-out))

#:extract  intrinsic-extract
#:wrap     intrinsic-wrap
#:parse    intrinsic-parse
#:compiler intrinsic-compiler

(define intrinsic-extract (const null))

(define intrinsic-wrap identity)

(define intrinsic-parse
  (syntax-parser
    [(prov:id
      ...)

     (for ([prov (in-list (attribute prov))])
       (bind-in-module! prov (intrinsic-var))
       (provide! prov prov))

     #f]))

(define intrinsic-compiler (const identity))
