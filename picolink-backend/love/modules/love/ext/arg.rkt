#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics

  #:template (#%member (#%member love arg) _)

  [parseGameArguments parse-game-arguments])
