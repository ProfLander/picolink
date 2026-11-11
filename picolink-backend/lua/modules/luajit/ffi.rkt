#lang picolink/r-lua/base

(require (only lua/ext/table  table-ref)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define ffi (s-lua (#%call require "ffi")))

(define c-def        (table-ref ffi "cdef"))
(define C            (table-ref ffi "C"))
(define load         (table-ref ffi "load"))
(define new          (table-ref ffi "new"))
(define type-of      (table-ref ffi "typeof"))
(define cast         (table-ref ffi "cast"))
(define metatype     (table-ref ffi "metatype"))
(define gc           (table-ref ffi "gc"))
(define size-of      (table-ref ffi "sizeof"))
(define align-of     (table-ref ffi "alignof"))
(define offset-of    (table-ref ffi "offsetof"))
(define is-type?     (table-ref ffi "istype"))
(define error-number (table-ref ffi "errno"))
(define string       (table-ref ffi "string"))
(define copy         (table-ref ffi "copy"))
(define fill         (table-ref ffi "fill"))
(define abi          (table-ref ffi "abi"))
(define os           (table-ref ffi "os"))
(define arch         (table-ref ffi "arch"))
