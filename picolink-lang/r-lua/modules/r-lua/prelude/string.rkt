#lang picolink/r-lua/base

(require (only lua error)
         (only lua/ext/type string?)
         (only lua/ext/values foldl-values)
         (only lua/string sub)

         r-lua/prelude/arith
         r-lua/prelude/condition
         r-lua/prelude/definition
         r-lua/prelude/equality
         r-lua/prelude/logic
         r-lua/prelude/s-lua)

(provide (all-defined)
         string?)

(define (%cat a b)
  (s-lua (#%cat a b)))

(define (string-length self)
  (s-lua (#%length self)))

(define (string-empty? self)
  (eq? self ""))

(define (substring self from to)

  (when (< from 0)
    (error "substring start index may not be negative"))

  (when (or (< (string-length self) from)
            (< (string-length self) to))
    (error "substring indices may not be greater than string length"))

  (when (< to from)
    (error
     "substring end index must be less than or equal to start index"))

  (sub self (+ from 1) to))

(define string-append
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values %cat #%vararg))))))
