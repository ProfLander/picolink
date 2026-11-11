#lang picolink/r-lua/base

(require (only lua/ext/table  table-ref)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define new   (s-lua (#%call require "table.new")))
(define clear (s-lua (#%call require "table.clear")))
