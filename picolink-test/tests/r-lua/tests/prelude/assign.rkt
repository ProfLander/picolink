#lang picolink/r-lua

(define a 1)
(set! a 2)
(print a)

(define b 3)
(set! b (if #t 4 5))
