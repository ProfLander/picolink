#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/interface/monoid  <>)

         r-lua/prelude/string)

(print "string monoid:")
(<> "foo" "bar" "baz")
