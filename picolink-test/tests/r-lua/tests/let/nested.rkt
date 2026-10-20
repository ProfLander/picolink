#lang picolink/r-lua

(require (only prelude/function
               identity))

(let ([x 2])
    x
    (let ([y (identity x)])
      y))
