#lang picolink/r-lua/base

(require (only lua  get-metatable set-metatable!)

         (only lua/ext/ops  ==)

         (only r-lua/prelude/definition       define)
         (only r-lua/prelude/function         const)
         (only r-lua/prelude/table            table))

(provide void void:type void?)

(define void:type
  (table [__index (table [Copy     #t]
                         [Drop     #t]
                         [Equal    #t]
                         [ToString #t]
                         [%to-string (const "#<void>")])]))

(define %void
  (set-metatable! (table)
                  void:type))

(define void
  (const %void))

(define (void? v)
  (== v %void))
