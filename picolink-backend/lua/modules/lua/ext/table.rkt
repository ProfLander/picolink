#lang picolink/r-lua/base

(require prelude/surface)

(provide (all-defined))

(define (member t k)
  (s-lua (#%member t (quote k))))

(define (length t)
  (s-lua (#%length t)))
