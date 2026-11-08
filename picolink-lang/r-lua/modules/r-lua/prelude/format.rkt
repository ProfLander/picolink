#lang picolink/r-lua/base

(require (rename lua/string  format %format)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define format
  (s-lua
   (#%function (str #%vararg)
               (#%block
                (#%local [str] [(#%call (#%member string gsub)
                                        str "~a" "%%s")])
                (#%return (#%call %format str #%vararg))))))
