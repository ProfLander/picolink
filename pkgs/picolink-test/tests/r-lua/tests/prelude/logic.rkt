#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/logic  and or not)

         r-lua/prelude/boolean
         r-lua/prelude/string)

(print "and:")
(and #t #f "nope")

(print "or:")
(or #f #f "yes")

(print "not:")
(not "hello")
