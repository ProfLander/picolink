#lang picolink/r-lua/base

(require (only lua  get-metatable)

         (only lua/ext/debug   debug-get-metatable debug-set-metatable!)
         (only lua/ext/object  field set-field!)

         (only luajit/ffi cast)

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
(debug-set-metatable! %inst CData)

(impl [Equal CData]
      (define equal?
        (field %CData "__eq")))

(impl [Order CData]
      (define less-than?
        (field %CData "__lt"))

      (define less-or-equal?
        (field %CData "__le")))

(impl [Length CData]
      (define length
        (field %CData "__len")))

(impl [Monoid CData]
      (define mappend
        (field %CData "__concat")))

(impl [Add CData]
      (define add
        (field %CData "__add")))

(impl [Subtract CData]
      (define subtract
        (field %CData "__sub")))

(impl [Multiply CData]
      (define multiply
        (field %CData "__mul")))

(impl [Divide CData]
      (define divide
        (field %CData "__div")))

(impl [Negate CData]
      (define negate
        (field %CData "__unm")))

(impl [Exponent CData]
      (define expt
        (field %CData "__pow")))

(impl [Modulo CData]
      (define modulo
        (field %CData "__mod")))

(impl [ToString CData]
      (define to-string
        (field %CData "__tostring")))

(set-field! CData "__index" (field %CData "__index"))
(set-field! CData "__newindex" (field %CData "__newindex"))
