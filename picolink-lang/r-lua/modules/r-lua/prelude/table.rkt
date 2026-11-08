#lang picolink/r-lua/base

(require (only r-lua/prelude/binding  let)
         (only r-lua/prelude/s-lua    s-lua))

(provide (all-defined))

(define-macro table
  [(_ [k v] ...)

   (with-syntax ([(k-str ...) (map (compose symbol->string
                                            syntax-e)
                                   (attribute k))]

                 [(v-ident ...) (map (λ (_) (generate-temporary))
                                   (attribute v))])

     (syntax/loc this-syntax
       (let ([v-ident v] ...)
         (s-lua
          (#%table [k-str v-ident] ...)))))]

  [(_ v ...)

   (with-syntax ([(v-ident ...) (map (λ (_) (generate-temporary))
                                   (attribute v))])

     (syntax/loc this-syntax
       (let ([v-ident v] ...)
         (s-lua
          (#%table v-ident ...)))))])
