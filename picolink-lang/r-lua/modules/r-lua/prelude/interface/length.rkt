#lang picolink/r-lua/base

(require (only lua/ext/metatable        metatable-send)
         (only r-lua/prelude/interface  define-interface impl)
         (only r-lua/prelude/print      print)
         (only r-lua/prelude/s-lua      s-lua))

(provide (all-defined))

(define-interface Length
  [length __length])

(impl [Length String]
      (define (length self)
        (s-lua (#%length self))))
