#lang picolink/r-lua/base

(require (only r-lua/runtime/print       print)
         (only r-lua/prelude/definition  define)

         r-lua/prelude/string)

(provide (all-defined))

(define (%mac val)
  (print "%mac:")
  (print val))
