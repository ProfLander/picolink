#lang picolink/r-lua/base

(require r-lua/prelude/interface)

(provide (all-defined))

(define-interface Update
  [update __update])

(define-interface Update!
  [update! __update!])
