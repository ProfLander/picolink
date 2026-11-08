#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define values
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return #%vararg)))))
