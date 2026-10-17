#lang picolink/r-lua

(require (in lua print)
         
         (in prelude/binding
             let*)

         (in prelude/conditional
             when
             unless))

(let* ([x 1]
       [y x])
  y)

(when #t
  (print "foo")
  "hello")

(unless #f
  (print "bar")
  "world")
