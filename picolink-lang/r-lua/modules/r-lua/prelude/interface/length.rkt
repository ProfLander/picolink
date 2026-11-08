#lang picolink/r-lua/base

(require (only lua/ext/metatable        metatable-send)
         (only r-lua/prelude/interface  define-interface))

(provide (all-defined))

(define-interface Length
  [length __length])
