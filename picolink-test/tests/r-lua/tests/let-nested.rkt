#lang picolink/r-lua

(require (in function identity))

(let ([x 6])
    x
    (let ([y (identity x)])
      y))
