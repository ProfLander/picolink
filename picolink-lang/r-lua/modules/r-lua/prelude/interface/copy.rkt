#lang picolink/r-lua/base

(require (only r-lua/prelude/interface  define-interface impl)
         (only r-lua/prelude/function   identity)
         (only r-lua/prelude/print      print)
         (only r-lua/prelude/type       Boolean Number String Procedure))

(provide (all-defined))

(define-interface Copy
  [copy __copy])

(impl [Copy Boolean]
      (define copy identity))

(impl [Copy Number]
      (define copy identity))

(impl [Copy String]
      (define copy identity))

(impl [Copy Procedure]
      (define copy identity))
