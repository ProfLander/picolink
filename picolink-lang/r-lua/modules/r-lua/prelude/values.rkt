#lang picolink/r-lua/base

(require r-lua/prelude/definition
         r-lua/prelude/s-lua)

(provide (all-defined))

(define values
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return #%vararg)))))
