#lang picolink/r-lua

(print "(list? (cons 1 2)):")
(list? (cons 1 2))

(print "(list? (cons null 1)):")
(list? (cons null 1))

(print "(list? (cons 1 null)):")
(list? (cons 1 null))

(print "(list 1 2 3 4):")
(list 1 2 3 4)

(define cell (cons (cons 1 2) (cons 2 (cons 3 4))))

(print "cell:")
cell

(print "(null? cell):")
(null? cell)

(print "(pair? cell):")
(pair? cell)

(print "(car cell):")
(car cell)

(print "(cdr cell):")
(cdr cell)

(print "(cadr cell):")
(cadr cell)

(print "(cddr cell):")
(cddr cell)
