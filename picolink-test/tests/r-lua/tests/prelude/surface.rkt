#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/assignment  set!)
         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/sequencing  begin)

         ; FIXME: Should be implicitly imported by lambda
         (only r-lua/prelude/list        list)

         r-lua/prelude/string
         r-lua/prelude/number)

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
