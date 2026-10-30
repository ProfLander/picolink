#lang picolink/r-lua/base

(require r-lua/prelude/surface
         r-lua/prelude/void)

(provide (all-defined))

(define (table-ref t k)
  (s-lua (#%member t (quote k))))

(define table-set!
  (s-lua
   (#%function (t k v)
               (#%block
                (#%assign [(#%member t (quote k))] [v])
                (#%return %void)))))

(define table-call
  (s-lua
   (#%function (t k #%vararg)
               (#%block
                (#%return (#%call (#%member t 'k) t #%vararg))))))

(define (table-length t)
  (s-lua (#%length t)))
