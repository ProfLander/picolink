#lang picolink/r-lua

(require (only lua/string
               upper
               lower
               format))

(define hello (upper "hello"))
(define world (lower "WORLD"))

(format "%s %s" hello world)
