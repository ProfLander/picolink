#lang picolink/lambda

(require (in lua
             print)
         (in lua/string
             upper
             lower))

(print (upper "hello,"))
(print (lower "WORLD!"))
