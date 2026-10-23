#lang picolink/r-lua

(require (only lua print))

(print "and:")
(and #t #f "nope")

(print "or:")
(or #f #f "yes")

(print "not:")
(not "hello")
