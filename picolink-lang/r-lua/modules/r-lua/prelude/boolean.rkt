#lang picolink/r-lua/base

(require (only lua/ext/type  boolean?)

         (only r-lua/prelude/function         identity)
         (only r-lua/prelude/interface        impl)
         (only r-lua/prelude/interface/copy   Copy)
         (only r-lua/prelude/interface/drop   Drop)
         (only r-lua/prelude/interface/equal  Equal)
         (only r-lua/prelude/print            print)
         (only r-lua/prelude/s-lua            s-lua)
         (only r-lua/prelude/type             Boolean)
         (only r-lua/prelude/void             void))

(provide boolean?)

(impl [Copy Boolean]
      (define copy identity))

(impl [Drop Boolean]
      (define drop void))

(impl [Equal Boolean]
      (define (equal? a b)
        (s-lua (#%eq a b))))
