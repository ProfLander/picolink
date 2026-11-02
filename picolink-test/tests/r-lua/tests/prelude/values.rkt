#lang picolink/r-lua

(let-values ([(a b c) (values 1 2 3)])
  (+ a b c))
