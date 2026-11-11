#lang picolink/r-lua/base

(require (only lua/ext/type number?)

         (only r-lua/prelude/function             identity)
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
         (only r-lua/prelude/print                print)
         (only r-lua/prelude/s-lua                s-lua)
         (only r-lua/prelude/type                 Number)
         (only r-lua/prelude/void                 void))

(provide number?)

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
(impl [ToString Number])
