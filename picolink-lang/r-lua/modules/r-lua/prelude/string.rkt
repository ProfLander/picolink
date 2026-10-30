#lang picolink/r-lua/base

(require (only lua type)
         (only lua/ext/values foldl-values)
         r-lua/prelude/equality
         r-lua/prelude/surface)

(provide (all-defined))

(define (string? v)
  (eq? (type v) "string"))

(define (%cat a b)
  (s-lua (#%cat a b)))

(define string-append
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return
                 (#%call foldl-values %cat #%vararg))))))
