#lang picolink/r-lua

(let ([x 2])
    x
    (let ([y (identity x)])
      y))
