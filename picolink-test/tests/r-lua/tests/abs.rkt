#lang picolink/r-lua

(define print-truthy
  (λ (key)
    (if key
        "true"
        "false")))

(print-truthy #t)
(print-truthy #f)
