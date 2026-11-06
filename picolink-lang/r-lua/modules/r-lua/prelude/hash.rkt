#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define-macro %hash
  [(_ (~seq k v) ...)

   (with-syntax ([(k-str ...) (map (compose symbol->string
                                            syntax-e)
                                   (attribute k))]

                 [(v-key ...) (map (λ (_) (generate-temporary))
                                   (attribute v))])

     (syntax/loc this-syntax
       (let ([k k-str] ...
             [v-key v] ...)
         (s-lua
          (#%table [k v] ...)))))])
