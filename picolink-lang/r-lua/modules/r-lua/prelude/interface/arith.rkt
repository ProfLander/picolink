#lang picolink/r-lua/base

(require (only lua/ext/ops  + sub neg * / ^ %)

         (only r-lua/prelude/interface        define-interface)
         (only r-lua/prelude/s-lua            s-lua))

(provide (all-defined))

(define-interface Add
  [add %add +])

(define-interface Subtract
  [subtract %subtract sub])

(define-interface Negate
  [negate %negate neg])

(define-interface Multiply
  [multiply %multiply *])

(define-interface Divide
  [divide %divide /])

(define-interface Exponent
  [expt %expt ^])

(define-interface Modulo
  [modulo %modulo %])
