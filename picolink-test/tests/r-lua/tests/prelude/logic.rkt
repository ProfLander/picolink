#lang picolink/r-lua

(print "and:")
(and #t #f "nope")

(print "or:")
(or #f #f "yes")

(print "not:")
(not "hello")
