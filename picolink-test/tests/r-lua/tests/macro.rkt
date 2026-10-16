#lang picolink/r-lua

(require (in lua print)
         (in macros
             let*
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
