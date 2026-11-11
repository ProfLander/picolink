#lang picolink/r-lua/base

(require (only lua/ext/table  table-ref)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define jit (s-lua (#%call require "jit")))

(define on          (table-ref jit "on"))
(define off         (table-ref jit "off"))
(define flush       (table-ref jit "flush"))
(define version     (table-ref jit "version"))
(define version-num (table-ref jit "version-num"))
(define os          (table-ref jit "os"))
(define arch        (table-ref jit "arch"))

(define opt         (table-ref jit "opt"))
(define start       (table-ref opt "start"))
