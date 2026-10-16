#lang picolink/r-lua

(provide let*
         when
         unless
         void)

(define %void
  "#<void>")

(define-macro void
  [(_ body ...)
   #'(begin
       body ...
       %void)])

(void 1234)

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
