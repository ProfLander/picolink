#lang picolink/r-lua/base

(require (only lua/runtime/global  emplace-globals!)
         (only lua/runtime/io      set-unbuffered-stdio!)
         (only lua/runtime/repl    repl)

         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (lua-pre module-name)
  (emplace-globals!)
  (set-unbuffered-stdio!))

(define (lua-post module-name)
  (repl module-name))
