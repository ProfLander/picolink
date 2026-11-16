#lang picolink/r-lua/base

(require (only lua  set-metatable!)

         (only lua/ext/ops  ->)
         (only lua/ext/type    number?)
         (only lua/ext/debug   debug-set-metatable!)
         
         (only r-lua/runtime/pool/type    Pooled)
         (only r-lua/runtime/pool/handle  handle?)
         (only r-lua/runtime/void         void)

         (only r-lua/prelude/binding              let)
         (only r-lua/prelude/function             identity)
         (only r-lua/prelude/if                   if)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/arith
               Add
               Subtract
               Negate
               Multiply
               Divide
               Exponent
               Modulo)
         (only r-lua/prelude/interface/copy       Copy)
         (only r-lua/prelude/interface/drop       Drop)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/order      Order)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/table                table)
         (only r-lua/prelude/type                 define-type))

(provide Number
         Number?
         number?)

; Basic type

(define-type Number)

(impl [Copy     Number])
(impl [Drop     Number])
(impl [Equal    Number])
(impl [Order    Number])
(impl [Add      Number])
(impl [Subtract Number])
(impl [Negate   Number])
(impl [Multiply Number])
(impl [Divide   Number])
(impl [Exponent Number])
(impl [Modulo   Number])

(impl [ToString Number]
      (define to-string identity))

; Number / Handle dispatcher

(define-type %Number
  #:index
  (λ (self key)

    (let* ([mt (if (handle? self)
                   Pooled
                   Number)]

           [out (-> (debug-set-metatable! self mt) key)])

      (debug-set-metatable! self %Number)

      out)))

(void (debug-set-metatable! 0 %Number))
