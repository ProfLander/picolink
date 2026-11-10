#lang picolink/r-lua/base

(require (only r-lua/prelude/interface  define-interface impl)
         (only r-lua/prelude/function   identity)
         (only r-lua/prelude/print      print)
         (only r-lua/prelude/type       Boolean Number String Procedure))

(provide (all-defined))

(define-interface Drop
  [drop __drop])

(impl [Drop Boolean]
      (define drop identity))

(impl [Drop Number]
      (define drop identity))

(impl [Drop String]
      (define drop identity))

(impl [Drop Procedure]
      (define drop identity))
