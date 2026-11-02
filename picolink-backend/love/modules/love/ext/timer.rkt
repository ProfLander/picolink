#lang picolink/r-lua/base

(require intrinsic)

(provide (all-defined))

(define-intrinsics

  #:template (#%member (#%member love timer) _)

  [getAverageDelta get-average-delta]
  [getDelta        get-delta]
  [getFPS          get-fps]
  [getTime         get-time]
  sleep
  step)
