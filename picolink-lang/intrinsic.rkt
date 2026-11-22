#lang picolink/language

(require racket/base
         racket/function
         
         syntax/parse

         picolink/scope
         picolink/intrinsic/var)

(provide (all-defined-out))

#:extract  intrinsic-extract
#:wrap     intrinsic-wrap
#:compiler intrinsic-compiler

(define intrinsic-extract (const null))

(define intrinsic-wrap identity)

(define intrinsic-compiler
  (syntax-parser
    [(prov:id
      ...)

     (for ([prov (in-list (attribute prov))])
       (bind! prov (intrinsic-var))
       (provide! prov prov))

     #f]))
