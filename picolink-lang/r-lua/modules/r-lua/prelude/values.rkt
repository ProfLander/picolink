#lang picolink/r-lua/base

(require (only lua  select)

         (only lua/ext/math  +)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (values . [: vs #%values])
  vs)

(define (values-length . [: vs #%values])
  (select "#" vs))

(define (values-skip idx . [: vs #%values])
  (select (+ 1 idx) vs))

(define (values-ref idx . [: vs #%values])
  (let ([val (values-skip idx vs)])
    val))
