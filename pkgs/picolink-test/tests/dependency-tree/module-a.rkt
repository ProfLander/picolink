#lang picolink/dsl

(require (only fst/module-b b)
         (only fst/module-c c))

(provide (all-from fst/module-b)
         (all-from fst/module-c)
         (all-defined))

(define a (let ([x b])
            (let ([y c])
              y)))
