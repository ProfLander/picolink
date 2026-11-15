#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua)
         (only r-lua/prelude/void        void))

(provide (all-defined))

(define (field t k)
  (s-lua (#%member t (quote k))))

(define set-field!
  (s-lua
   (#%function (t k v)
               (#%block
                (#%assign [(#%member t (quote k))] [v])
                (#%return (#%call void))))))

(define (send t k . [: vs #%values])
  ((field t k) t vs))
