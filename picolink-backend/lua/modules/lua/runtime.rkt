#lang picolink/r-lua/base

(require (only lua/runtime/io
               set-unbuffered-stdio!)

         (only lua/runtime/repl
               repl)

         r-lua/prelude/definition
         r-lua/prelude/print)

(provide (all-defined))

(define (lua-pre module-name)
  (set-unbuffered-stdio!))

(define (lua-post module-name)
  (repl module-name))
