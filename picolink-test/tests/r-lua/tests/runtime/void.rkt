#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)
         (only r-lua/runtime/void   void)

         (only r-lua/prelude/function  identity)

         r-lua/prelude/string)

(void)

(void 1 2)

(void "foo" "bar")

(void (print (identity "baz"))
      (identity "decafisbad"))
