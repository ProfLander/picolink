#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/print       print)
         (only r-lua/prelude/void        void)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define emplace-globals!
  (s-lua (#%function ()
                     (#%block
                      (#%assign [(#%member _G print)
                                 (#%member _G void)]

                                [print
                                 void])))))
