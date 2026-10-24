#lang picolink/r-lua

(let ([x 4]
      [y (let ([x 3])
           (identity x))])
  (print y)
  x)
