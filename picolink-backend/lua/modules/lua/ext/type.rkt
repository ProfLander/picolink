#lang picolink/r-lua/base

(provide (all-defined))

(define nil?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "nil"))))

(define boolean?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "boolean"))))

(define number?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "number"))))

(define string?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "string"))))

(define userdata?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "userdata"))))

(define function?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "function"))))

(define thread?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "thread"))))

(define table?
  (λ (v)
    (#%intrinsic (#%eq (#%call type v) "table"))))
