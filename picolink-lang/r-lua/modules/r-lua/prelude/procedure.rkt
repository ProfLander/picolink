#lang picolink/r-lua/base

(require (only lua/ext/type  function?)

         (only r-lua/prelude/function         identity)
         (only r-lua/prelude/interface        define-interface impl)
         (only r-lua/prelude/interface/copy   Copy)
         (only r-lua/prelude/interface/drop   Drop)
         (only r-lua/prelude/interface/equal  Equal)
         (only r-lua/prelude/print            print)
         (only r-lua/prelude/type             Procedure)
         (only r-lua/prelude/void             void))

(provide (all-defined)
         (rename function? procedure?))

(impl [Copy Procedure]
      (define copy identity))

(impl [Drop Procedure]
      (define drop void))

(impl [Equal Procedure]
      (define (equal? a b)
        (s-lua (#%eq a b))))

(define-interface Apply
  [apply __call
         (λ (self . [: vs #%values])
           (self vs))])
