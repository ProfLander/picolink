#lang picolink/r-lua/base

(require (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define-macro and

  [(_ a rest ...+)
   (syntax/loc this-syntax
     (if a
         (and rest ...)
         #f))]

  [(_ a)
   #'a])

(define-macro or

  [(_ a rest ...+)
   (with-syntax ([a* (generate-temporary)])
     (syntax/loc this-syntax
       (let ([a* a])
         (if a*
             a*
             (or rest ...)))))]

  [(_ a)
   #'a])

(define (not v)
  (s-lua (#%not v)))

