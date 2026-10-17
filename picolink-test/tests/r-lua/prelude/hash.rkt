#lang picolink/r-lua

(provide %hash)

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
         (#%intrinsic
          (#%table [k v] ...))))])
