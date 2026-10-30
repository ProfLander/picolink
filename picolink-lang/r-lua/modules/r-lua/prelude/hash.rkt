#lang picolink/r-lua/base

(provide (all-defined))

(define-macro %hash
  [(_ (~seq k v) ...)
   (with-syntax ([(k-str ...) (map (compose symbol->string
                                            syntax-e)
                                   (attribute k))]
                 [(v-key ...) (map (λ (_) (generate-temporary))
                                   (attribute v))])
     #'(let ([k k-str]
             ...
             [v-key v]
             ...)
         (s-lua
          (#%table [k v] ...))))])
