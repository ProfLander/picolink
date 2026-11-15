#lang picolink/r-lua/base

(require (rename lua  to-string lua-to-string)

         (only r-lua/prelude/interface  define-interface))

(provide (all-defined))

(define-interface ToString
  #:raw
  [to-string __tostring
             (λ (a)
               (lua-to-string a))])
