#lang picolink/r-lua/base

(require (only lua
               newproxy)

         (only lua/ext/table
               table-set!)

         (only lua/ext/metatable
               get-metatable))

(require r-lua/prelude/binding
         r-lua/prelude/definition
         r-lua/prelude/function
         r-lua/prelude/s-lua)

(provide (all-defined))

(define %void
  (newproxy #t))

(define void:type
  (let ([mt (get-metatable %void)])
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
  (s-lua (#%eq v %void)))
