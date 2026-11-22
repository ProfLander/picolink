#lang picolink/r-lua/base

(require (only luajit/table  new clear)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define make-table  new)

(define (make-array-table n)
  (make-table n 0))

(define (make-hash-table n)
  (make-table 0 n))

(define clear-table clear)

