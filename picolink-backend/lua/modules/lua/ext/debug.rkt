#lang picolink/r-lua/base

(require (only lua/debug  get-metatable set-metatable!)

         (only r-lua/runtime/void        void)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define debug-get-metatable get-metatable)

(define (debug-set-metatable! t mt)
  (set-metatable! t mt)
  t)
