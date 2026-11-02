#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined))

(define values
  (s-lua
   (#%function (#%vararg)
               (#%block
                (#%return #%vararg)))))
