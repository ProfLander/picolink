#lang picolink/r-lua/base

(require prelude/surface)

(provide (all-defined))

(define (nil? v)
  (#%intrinsic (#%eq (#%call type v) "nil")))

(define (boolean? v)
  (#%intrinsic (#%eq (#%call type v) "boolean")))

(define (number? v)
  (#%intrinsic (#%eq (#%call type v) "number")))

(define (string? v)
  (#%intrinsic (#%eq (#%call type v) "string")))

(define (userdata? v)
  (#%intrinsic (#%eq (#%call type v) "userdata")))

(define (function? v)
  (#%intrinsic (#%eq (#%call type v) "function")))

(define (thread? v)
  (#%intrinsic (#%eq (#%call type v) "thread")))

(define (table? v)
  (#%intrinsic (#%eq (#%call type v) "table")))
