#lang picolink/r-lua

(require (only-in lua
                  print)

         (only-in prelude/function
                  identity))

(let ([x 2]
      [y (let ([x 3])
           (identity x))])
  (print x)
  y)
