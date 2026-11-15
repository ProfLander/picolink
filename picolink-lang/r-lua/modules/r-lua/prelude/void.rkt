#lang picolink/r-lua/base

(require (only lua  get-metatable set-metatable!)

         (only r-lua/prelude/definition       define)
         (only r-lua/prelude/function         const)
         (only r-lua/prelude/s-lua            s-lua)
         (only r-lua/prelude/table            table))

(provide void void:type void?)

(define void:type
  (table [ToString #t]
         [Equal #t]
         [__index (table)]
         [__tostring (const "#<void>")]))

(define %void
  (set-metatable! (table)
                  void:type))

(define void
  (const %void))

(define (void? v)
  (s-lua (#%eq v %void)))
