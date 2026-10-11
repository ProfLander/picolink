#lang picolink/r-lua

(define-macro let*
  [(_ (bind ...) body ...)
   (for/foldr ([acc #'(begin body ...)])
              ([bind (in-list (attribute bind))])
     #`(let (#,bind)
         #,acc))])

(let* ([x 1]
       [y x])
  y)
