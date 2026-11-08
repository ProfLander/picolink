#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define send
  (s-lua
   (#%function (t k #%vararg)
               (#%block
                (#%return (#%call (#%member t 'k) t #%vararg))))))
