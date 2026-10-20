#lang picolink/r-lua

(provide (all-defined))

(define-macro let*
  [(_ (bind ...) body ...)
   (for/foldr ([acc #'(begin body ...)])
              ([bind (in-list (attribute bind))])
     #`(let (#,bind)
         #,acc))])
