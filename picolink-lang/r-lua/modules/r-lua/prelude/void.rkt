#lang picolink/r-lua/base

(require (only lua/ext/table      table-set!)
         (only lua/ext/metatable  get-metatable)
         (only lua/ext/proxy      make-proxy)

         (only r-lua/prelude/binding          let)
         (only r-lua/prelude/definition       define)
         (only r-lua/prelude/function         const)
         (only r-lua/prelude/s-lua            s-lua))

(provide (all-defined))

(define %void
  (make-proxy #t))

(define void:type
  (let ([mt (get-metatable %void)])

    ; Manual ToString implementation
    (table-set! mt
                "%to-string"
                (const "#<void>"))

    ; Manual Equal implementation
    (table-set! mt
                "%equal?"
                (λ (a b)
                  (s-lua (#%eq a b))))

    mt))

(define void
  (const %void))

(define (void? v)
  (s-lua (#%eq v %void)))
