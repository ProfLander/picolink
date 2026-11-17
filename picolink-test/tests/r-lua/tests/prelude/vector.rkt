#lang picolink/r-lua/base

(require (only r-lua/runtime/print  print)

         (only r-lua/prelude/definition        define)
         (only r-lua/prelude/interface/length  length)
         (only r-lua/prelude/list              list)
         (only r-lua/prelude/sequence          in-range)
         (only r-lua/prelude/vector
               vector
               vector-ref
               vector->list
               list->vector
               for/vector)

         r-lua/prelude/number
         r-lua/prelude/string)

(define inst (vector 2 4 6 8))

(print "inst:")
inst

(print "(length inst):")
(length inst)

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
