#lang picolink/dsl

(require fst/module-b
         fst/module-c)

(provide (all-from fst/module-b)
         (all-from fst/module-c)
         (all-defined))

(define a (let ([x b])
            (let ([y c])
              y)))
