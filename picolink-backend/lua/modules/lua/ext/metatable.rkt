#lang picolink/r-lua/base

(require (only lua
               getmetatable
               setmetatable)

         (only lua/ext/table
               table-ref
               table-set!)

         r-lua/prelude/definition
         r-lua/prelude/s-lua)

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

(define metatable-send
  (s-lua
   (#%function (self k #%vararg)
               (#%block
                (#%return
                 (#%call (#%call metatable-ref self k)
                         self #%vararg))))))
