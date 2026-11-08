#lang picolink/r-lua/base

(require (only lua/ext/metatable metatable-send)
         r-lua/prelude/interface)

(provide (all-defined))

(define-interface Length
  [length __length])
