#lang picolink/r-lua/base

(require (only lua/ext/table  table-ref)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define string-buffer (s-lua (#%call require "string.buffer")))

(define new    (table-ref string-buffer "new"))
(define encode (table-ref string-buffer "encode"))
(define decode (table-ref string-buffer "decode"))
