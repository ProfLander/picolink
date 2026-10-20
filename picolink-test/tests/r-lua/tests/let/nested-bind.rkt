#lang picolink/r-lua

(require (only lua
               print)

         (only prelude/function
               identity))

(let ([x 4]
      [y (let ([x 3])
           (identity x))])
  (print y)
  x)
