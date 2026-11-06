#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define-macro let*
  [(_ (bind ...) body ...)

   (for/foldr ([acc (quasisyntax/loc this-syntax
                      (begin body ...))])

              ([bind (in-list (attribute bind))])

     (quasisyntax/loc acc
       (let (#,bind)
         #,acc)))])
