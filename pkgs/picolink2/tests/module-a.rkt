#lang reader "../r-lua/reader.rkt"

(provide (all-defined-out))

; data

(define bool-false #f)
(define bool-true  #t)
(define number     1234)
(define string     "hello")

; abstraction

(define fst
  (λ (a b)
    a))

(define snd
  (λ (a b)
    b))

; binding

#;(define foo
    (let-values ([(a b c) (values 1 2 3)])
      (print a)
      (print b)
      (print c)))
