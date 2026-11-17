#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/lambda      λ)

         r-lua/prelude/string)

(define print-truthy
  (λ (key)
    (if key
        "true"
        "false")))

(print-truthy #t)
(print-truthy #f)
