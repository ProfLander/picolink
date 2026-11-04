#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define-macro and

  [(_ a rest ...+)
   #'(if a
         (and rest ...)
         #f)]

  [(_ a)
   #'a])

(define-macro or

  [(_ a rest ...+)
   (with-syntax ([a* (generate-temporary)])
     #'(let ([a* a])
         (if a*
             a*
             (or rest ...))))]

  [(_ a)
   #'a])

(define (not v)
  (s-lua (#%not v)))

