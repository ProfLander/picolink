#lang picolink/r-lua/base

(require (only lua type)
         (only lua/ext/table member)
         (only lua/ext/type table?)
         prelude/equality)

(provide (all-defined))

(define %tag "__tag")

(define tag?
  (λ (v)
    (eq? v %tag)))

(define tag
  (λ (v)
    (if (table? v)
        (member v "__tag")
        #f)))
