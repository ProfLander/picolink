#lang picolink/r-lua/base

(require (only lua  get-metatable)

         (only lua/ext/ops  -> =)
         (only lua/ext/debug   debug-get-metatable debug-set-metatable!)

         (only luajit/ffi cast)

         (only r-lua/runtime/void  void)

         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/equal      Equal)
         (only r-lua/prelude/interface/order      Order)
         (only r-lua/prelude/interface/arith
               Add
               Subtract
               Multiply
               Divide
               Negate
               Exponent
               Modulo)
         (only r-lua/prelude/interface/monoid     Monoid)
         (only r-lua/prelude/interface/length     Length)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/type                 define-type))

(provide (all-defined))

(define %inst (cast "uint64_t" 0))
(define %CData (debug-get-metatable %inst))

(define-type CData)
(void (debug-set-metatable! %inst CData))

(impl [Equal CData]
      (define equal?
        (-> %CData "__eq")))

(impl [Order CData]
      (define less-than?
        (-> %CData "__lt"))

      (define less-or-equal?
        (-> %CData "__le")))

(impl [Length CData]
      (define length
        (-> %CData "__len")))

(impl [Monoid CData]
      (define mappend
        (-> %CData "__concat")))

(impl [Add CData]
      (define add
        (-> %CData "__add")))

(impl [Subtract CData]
      (define subtract
        (-> %CData "__sub")))

(impl [Multiply CData]
      (define multiply
        (-> %CData "__mul")))

(impl [Divide CData]
      (define divide
        (-> %CData "__div")))

(impl [Negate CData]
      (define negate
        (-> %CData "__unm")))

(impl [Exponent CData]
      (define expt
        (-> %CData "__pow")))

(impl [Modulo CData]
      (define modulo
        (-> %CData "__mod")))

(impl [ToString CData]
      (define to-string
        (-> %CData "__tostring")))

(= CData "__index"    (-> %CData "__index"))
(= CData "__newindex" (-> %CData "__newindex"))
