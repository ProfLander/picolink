#lang picolink/dsl

(require (in fst/module-b b)
         (in fst/module-c c))

(provide a*)

(define a (let ([x b])
            (let ([y c])
              y)))

(define a* a)
