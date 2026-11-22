#lang picolink/r-lua/base

(require (only lua  %require)

         (only lua/ext/table    table-ref)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define utf8 (%require "utf8"))

(define char         (table-ref utf8 "char"))
(define char-pattern (table-ref utf8 "charpattern"))
(define codes        (table-ref utf8 "codes"))
(define codepoint    (table-ref utf8 "codepoint"))
(define length       (table-ref utf8 "len"))
(define offset       (table-ref utf8 "offset"))
