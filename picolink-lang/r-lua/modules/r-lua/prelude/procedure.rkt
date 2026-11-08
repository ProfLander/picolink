#lang picolink/r-lua/base

(require (only lua/ext/type function?)
         r-lua/prelude/interface)

(provide (all-defined)
         (rename function? procedure?))

(define-interface Apply
  [apply __call
         (s-lua (#%function (self #%vararg)
                            (#%block
                             (#%return
                              (#%call self #%vararg)))))])
