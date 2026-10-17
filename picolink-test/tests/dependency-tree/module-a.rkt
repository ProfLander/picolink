#lang picolink/dsl

(require (only-in fst/module-b [b* b])
         (only-in fst/module-c c))

(provide [a a*])

(define a (let ([x b])
            (let ([y c])
              y)))
