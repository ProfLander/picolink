#lang picolink/r-lua/base

(require prelude/surface
         prelude/void)

(provide (all-defined))

(define (table-ref t k)
  (s-lua (#%member t (quote k))))

(define table-set!
  (s-lua
   (#%function (t k v)
               (#%block
                (#%assign [(#%member t (quote k))] [v])
                (#%return %void)))))

(define (table-length t)
  (s-lua (#%length t)))
