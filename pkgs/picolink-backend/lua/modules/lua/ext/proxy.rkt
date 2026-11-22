#lang picolink/r-lua/base

(require (only lua newproxy)
         (only r-lua/prelude/definition  define))

(provide make-proxy)

(define (make-proxy [with-metatable #f])
  (newproxy with-metatable))
