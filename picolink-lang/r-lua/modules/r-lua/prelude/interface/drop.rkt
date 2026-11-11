#lang picolink/r-lua/base

(require (only r-lua/prelude/interface  define-interface)
         (only r-lua/prelude/void       void))

(provide (all-defined))

(define-interface Drop
  [drop %drop void])
