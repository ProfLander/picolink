#lang picolink/r-lua

(require (in lua print))

(define thing "define-thing")
thing
(set! thing "define-set")
thing

(define thing2 "begin-thing")
(begin
  (print thing2)
  (set! thing2 "begin-set")
  thing2)

(let ([thing "let-thing"])
  (print thing)
  (set! thing "let-set")
  thing)
