#lang picolink/r-lua/base

(require (only lua  %require)

         (only lua/ext/table    table-ref)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define bit (%require "bit"))

(define to-bit                 (table-ref bit "tobit"))
(define to-hex                 (table-ref bit "tohex"))
(define bitwise-not            (table-ref bit "bnot"))
(define bitwise-and            (table-ref bit "band"))
(define bitwise-or             (table-ref bit "bor"))
(define bitwise-xor            (table-ref bit "bxor"))
(define shift-left             (table-ref bit "lshift"))
(define shift-right            (table-ref bit "rshift"))
(define arithmetic-right-shift (table-ref bit "arshift"))
(define rotate-left            (table-ref bit "rol"))
(define rotate-right           (table-ref bit "ror"))
(define bit-swap               (table-ref bit "bswap"))
