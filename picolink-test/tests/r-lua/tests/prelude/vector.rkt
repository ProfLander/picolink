#lang picolink/r-lua

(define inst (vector 2 4 6 8))

(print "inst:")
inst

(print "(vector-length inst):")
(vector-length inst)

(print "(vector-ref inst 0):")
(vector-ref inst 0)

(print "(vector-ref inst 1):")
(vector-ref inst 1)

(print "(vector-ref inst 2):")
(vector-ref inst 2)

(print "(vector-ref inst 3):")
(vector-ref inst 3)
