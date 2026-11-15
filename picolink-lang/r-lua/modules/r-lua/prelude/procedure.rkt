#lang picolink/r-lua/base

(require (only lua/ext/type   function?)
         (only lua/ext/debug  debug-set-metatable!)

         (only r-lua/prelude/function         identity)
         (only r-lua/prelude/interface        define-interface impl)
         (only r-lua/prelude/interface/copy   Copy)
         (only r-lua/prelude/interface/drop   Drop)
         (only r-lua/prelude/interface/equal  Equal)
         (only r-lua/prelude/lambda           λ)
         (only r-lua/prelude/type             define-type)
         (only r-lua/prelude/void             void))

(provide (all-defined)
         (rename function? procedure?))

(define-type Procedure)
(debug-set-metatable! (λ (x) x) Procedure)

(impl [Copy Procedure]
      (define copy identity))

(impl [Drop Procedure]
      (define drop void))

(impl [Equal Procedure]
      (define (equal? a b)
        (s-lua (#%eq a b))))
