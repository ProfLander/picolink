#lang picolink/r-lua/base

(require (only lua type)

         (only lua/ext/ops ==)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (nil? v)
  (== (type v) "nil"))

(define (boolean? v)
  (== (type v) "boolean"))

(define (number? v)
  (== (type v) "number"))

(define (string? v)
  (== (type v) "string"))

(define (userdata? v)
  (== (type v) "userdata"))

(define (function? v)
  (== (type v) "function"))

(define (thread? v)
  (== (type v) "thread"))

(define (table? v)
  (== (type v) "table"))
