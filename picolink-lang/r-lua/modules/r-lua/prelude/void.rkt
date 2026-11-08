#lang picolink/r-lua/base

(require (only lua/ext/table      table-set!)
         (only lua/ext/metatable  get-metatable)
         (only lua/ext/proxy      make-proxy)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/function    const)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define %void
  (make-proxy #t))

(define void:type
  (let ([mt (get-metatable %void)])
    (table-set! mt
                "__tostring"
                (const "#<void>"))
    mt))

(define void
  (const %void))

(define (void? v)
  (s-lua (#%eq v %void)))
