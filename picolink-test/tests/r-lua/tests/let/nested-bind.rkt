#lang picolink/r-lua

(require (in lua print)
         (in prelude/function
             identity))

(let ([x 2]
      [y (let ([x 3])
           (identity x))])
  (print x)
  y)
