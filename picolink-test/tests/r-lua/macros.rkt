#lang picolink/r-lua

(provide let*
         when
         unless)

(define-macro let*
  [(_ (bind ...) body ...)
   (for/foldr ([acc #'(begin body ...)])
              ([bind (in-list (attribute bind))])
     #`(let (#,bind)
         #,acc))])

(define-macro when
  [(_ cond body ...)
   #'(if cond
         (begin body ...)
         (void))])

(define-macro unless
  [(_ cond body ...)
   #'(if cond
         (void)
         (begin body ...))])
