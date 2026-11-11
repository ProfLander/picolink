#lang picolink/r-lua/base

(require (only lua select)

         (only lua/ext/values foldl-values)

         (only r-lua/prelude/definition       define)
         (only r-lua/prelude/if               if)
         (only r-lua/prelude/interface        define-interface)
         (only r-lua/prelude/interface/equal  equal?)
         (only r-lua/prelude/interface/arith
               add
               subtract
               negate
               multiply
               divide
               expt
               modulo)
         (only r-lua/prelude/s-lua            s-lua))

(provide (all-defined))

(define = equal?)

(define (+ . [: vs #%values])
  (foldl-values add vs))

(define (- . [: vs #%values])
  (if (= (select "#" vs) 1)
      (negate vs)
      (foldl-values subtract vs)))

(define (* . [: vs #%values])
  (foldl-values multiply vs))

(define (/ . [: vs #%values])
  (foldl-values divide vs))
