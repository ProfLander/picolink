#lang picolink/r-lua/base

(require (only lua/string
               upper
               lower)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/format      format)

         r-lua/prelude/string)

(define hello (upper "hello"))
(define world (lower "WORLD"))

(format "~a ~a" hello world)
