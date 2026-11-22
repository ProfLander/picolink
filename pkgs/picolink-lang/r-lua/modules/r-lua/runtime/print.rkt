#lang picolink/r-lua/base

(require (rename lua print lua-print)

         (only r-lua/runtime/void                 void void?)

         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/if                   if)
         (only r-lua/prelude/interface/to-string  to-string))

(provide print)

(define (print v)
  (if (void? v)
      v
      (void (lua-print (to-string v)))))
