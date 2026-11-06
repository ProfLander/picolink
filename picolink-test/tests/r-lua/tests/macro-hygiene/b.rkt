#lang picolink/r-lua

(require tests/macro-hygiene/c)

(provide mac)

(define-macro mac
  [(_ body)
   #'(%mac body)])
