#lang picolink/r-lua

(require (only-in prelude/function
                  identity))

(let ([x 6])
    x
    (let ([y (identity x)])
      y))
