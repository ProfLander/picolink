#lang picolink/r-lua/base

(require (only lua newproxy))

(require r-lua/prelude/equality
         r-lua/prelude/surface)

(provide (all-defined))

(define %void
  (newproxy))

(define (void? v)
  (eq? v %void))

(define void
  (s-lua
   (#%function (#%vararg)
    (#%block
     (#%return %void)))))
