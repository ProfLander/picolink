#lang picolink/r-lua

(require (only lua
               print)
         
         (only prelude/binding
               let*)

         (only prelude/conditional
               when
               unless))

(let* ([x "foo"]
       [y x])
  y)

(when #t
  (print "bar")
  "hello")

(unless #f
  (print "baz")
  "world")
