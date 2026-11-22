#lang picolink/r-lua/base

(require (only lua  get-metatable set-metatable!)

         (only lua/ext/table  table-ref table-set!)

         (only r-lua/prelude/definition  define))

(provide metatable-ref
         metatable-set!
         metatable-send)

(define (metatable-ref t k)
  (table-ref (get-metatable t) k))

(define (metatable-set! t k v)
  (table-set! (get-metatable t) k v))

(define (metatable-send t k . [: vs #%values])
  ((metatable-ref t k) t vs))
