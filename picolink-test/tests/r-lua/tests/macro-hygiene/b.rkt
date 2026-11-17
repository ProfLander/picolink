#lang picolink/r-lua/base

(require tests/macro-hygiene/c)

(provide mac)

(define-macro mac
  [(_ body)
   #'(%mac body)])
