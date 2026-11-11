#lang picolink/r-lua/base

(require (only lua/ext/table  table-ref)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define bit (s-lua (#%call require "bit")))

(define to-bit                 (table-ref bit "tobit"))
(define to-hex                 (table-ref bit "tohex"))
(define bitwise-not            (table-ref bit "bnot"))
(define bitwise-and            (table-ref bit "band"))
(define bitwise-or             (table-ref bit "bor"))
(define bitwise-xor            (table-ref bit "bxor"))
(define left-shift             (table-ref bit "lshift"))
(define right-shift            (table-ref bit "rshift"))
(define arithmetic-right-shift (table-ref bit "arshift"))
(define rotate-left            (table-ref bit "rol"))
(define rotate-right           (table-ref bit "ror"))
(define bit-swap               (table-ref bit "bswap"))
