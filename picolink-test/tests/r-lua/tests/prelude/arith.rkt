#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/arith  + - * /)

         r-lua/prelude/number
         r-lua/prelude/string)

(print "+:")
(+ 1 2 3 4)

(print "-:")
(- 10 4 2 1)
(- 10)

(print "*:")
(* 2 2 2)

(print "/:")
(/ 8 2 2)
