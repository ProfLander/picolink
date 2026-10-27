#lang picolink/r-lua

(define thing "define-thing")
thing
(set! thing "define-set")
thing

(define thing2 "begin-thing")
(begin
  (print thing2)
  (set! thing2 "begin-set")
  thing2)

(let ([thing "let-thing"])
  (print thing)
  (set! thing "let-set")
  thing)

(define thing3 "tail-thing")
thing3
(begin
  (set! thing3 "tail-set"))
thing3

(define (dotted-varargs x . y)
  (print "arg:")
  (print x)
  (print "dotted varargs:")
  (print y))

(define (single-varargs . z)
  (print "single varargs:")
  (print z))

(dotted-varargs 1 2 3 4)
(single-varargs 1 2 3 4)
