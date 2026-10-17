#lang picolink/r-lua

(require (only-in lua
                  print)
         
         (only-in prelude/binding
                  let*)

         (only-in prelude/conditional
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
