#lang picolink/r-lua/base

(require (only lua/ext/type function?)
         r-lua/prelude/interface)

(provide (all-defined)
         (rename function? procedure?))

(define-interface Apply
  [apply __call
         (λ (self . [: vs #%values])
           (self vs))])
