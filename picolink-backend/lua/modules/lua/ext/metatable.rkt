#lang picolink/r-lua/base

(require (only lua  getmetatable setmetatable)

         (only lua/ext/table  table-ref table-set!)

         (only r-lua/prelude/definition  define))

(provide get-metatable
         set-metatable!
         metatable-ref
         metatable-set!
         metatable-send)

(define get-metatable getmetatable)
(define set-metatable! setmetatable)

(define (metatable-ref t k)
  (table-ref (get-metatable t) k))

(define (metatable-set! t k v)
  (table-set! (get-metatable t) k v))

(define (metatable-send t k . [: vs #%values])
  ((metatable-ref t k) t vs))
