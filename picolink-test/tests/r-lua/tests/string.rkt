#lang picolink/r-lua

(require (only lua/string
               upper
               lower))

(define hello (upper "hello"))
(define world (lower "WORLD"))

(format "~a ~a" hello world)
