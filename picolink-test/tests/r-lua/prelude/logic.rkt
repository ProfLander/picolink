#lang picolink/r-lua

(require (in lua print))

(provide and or not)

(define %and
  (λ (a b)
    (#%intrinsic (#%and a b))))

(define-macro and
  [(_ a b rest ...+)
   #'(let ([res (and a b)])
       (and res rest ...))]
  [(_ a b)
   #'(%and a b)])

(print "and:")
(and #t #f "nope")

(define %or
  (λ (a b)
    (#%intrinsic (#%or a b))))

(define-macro or
  [(_ a b rest ...+)
   #'(let ([res (or a b)])
       (or res rest ...))]
  [(_ a b)
   #'(%or a b)])

(print "or:")
(or #f #f "yes")

(define not
  (λ (v)
    (#%intrinsic (#%not v))))

(print "not:")
(not "hello")
