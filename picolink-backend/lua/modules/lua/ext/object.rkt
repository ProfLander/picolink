#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define send
  (s-lua
   (#%function (t k #%vararg)
               (#%block
                (#%return (#%call (#%member t 'k) t #%vararg))))))
