#lang picolink/r-lua

(require (only lua print))

(let ([x 4]
      [y (let ([x 3])
           (identity x))])
  (print y)
  x)
