#lang picolink/r-lua/base

(require prelude/surface)

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
   #'(if a
         a
         (or rest ...))]

  [(_ a)
   #'a])

(define (not v)
  (s-lua (#%not v)))

