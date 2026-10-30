#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define (nil? v)
  (s-lua (#%eq (#%call type v) "nil")))

(define (boolean? v)
  (s-lua (#%eq (#%call type v) "boolean")))

(define (number? v)
  (s-lua (#%eq (#%call type v) "number")))

(define (string? v)
  (s-lua (#%eq (#%call type v) "string")))

(define (userdata? v)
  (s-lua (#%eq (#%call type v) "userdata")))

(define (function? v)
  (s-lua (#%eq (#%call type v) "function")))

(define (thread? v)
  (s-lua (#%eq (#%call type v) "thread")))

(define (table? v)
  (s-lua (#%eq (#%call type v) "table")))
