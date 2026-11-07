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

(print "for/vector:")
(for/vector ([i (in-range 5 0 -1)])
  i)

(print "vector->list:")
(vector->list (vector 1 3 5 7 9))

(print "list->vector:")
(list->vector (list 2 4 6 8 10))
