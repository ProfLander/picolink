#lang picolink/r-lua/base

(require (only luajit/table  new clear)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define make-table  new)
(define clear-table clear)

