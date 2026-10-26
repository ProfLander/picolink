#lang picolink/r-lua/base

(require (only lua/ext/table member)
         (only lua/ext/type table?)
         prelude/surface
         prelude/equality)

(provide (all-defined))

(define %tag "__tag")

(define (tag? v)
  (eq? v %tag))

(define (tag v)
  (if (table? v)
      (member v "__tag")
      #f))
