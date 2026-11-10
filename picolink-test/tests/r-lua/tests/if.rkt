#lang picolink/r-lua

(if #t 1234 5678)

(if #f 1234 5678)

(identity
 (if #f
     "a"
     "b"))

#;(let-values ([(a b) (if #t (values 1 2) (values 3 4))])
    (print "values:")
    (print a)
    (print b))
