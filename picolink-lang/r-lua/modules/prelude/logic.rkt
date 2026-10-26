#lang picolink/r-lua/base

(require prelude/surface)

(provide (all-defined))

(define-macro and
  [(_ a rest ...+)
   #'(let ([tail (and rest ...)])
       (if tail
           a
           #f))]
  [(_ a)
   #'(if a a #f)])

(define-macro or
  [(_ a rest ...+)
   #'(let ([tail (or rest ...)])
       (if a a tail))]
  [(_ a)
   #'a])

(define (not v)
  (#%intrinsic (#%not v)))

