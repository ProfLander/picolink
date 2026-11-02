#lang picolink/r-lua/base

(require (only lua/io read)
         (only lua/ext/object send)
         (only love/ext/thread get-channel)
         r-lua/prelude/surface)

(define channel (get-channel "stdin"))

(define (loop)
  (let ([input (read)])
    (send channel "push" input))
  (loop))

(loop)
