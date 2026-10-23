#lang picolink/r-lua

(require (only lua print))

(define cell (cons 1 (cons 2 null)))

(print "cell:")
cell

(print "null?:")
(null? cell)

(print "pair?:")
(pair? cell)

(print "car:")
(car cell)

(print "cdr:")
(cdr cell)

(print "cadr:")
(cadr cell)

(print "cddr:")
(cddr cell)
