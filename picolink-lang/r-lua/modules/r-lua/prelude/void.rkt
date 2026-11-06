#lang picolink/r-lua/base

(require (only lua
               newproxy
               getmetatable)

         (only lua/ext/table
               table-set!))

(require r-lua/prelude/binding
         r-lua/prelude/definition
         r-lua/prelude/equality
         r-lua/prelude/function
         r-lua/prelude/s-lua)

(provide (all-defined))

(define %void
  (newproxy #t))

(define void:type
  (let ([mt (getmetatable %void)])
    (table-set! mt
                "__tostring"
                (const "#<void>"))
    mt))

(define void
  (s-lua
   (#%function (#%vararg)
    (#%block
     (#%return %void)))))

(define (void? v)
  (eq? v %void))
