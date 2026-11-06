#lang picolink/r-lua/base

(require (only lua/io read)
         (only lua/ext/object send)
         (only love/ext/thread get-channel)
         
         r-lua/prelude/binding
         r-lua/prelude/definition)

(define channel (get-channel "stdin"))

(define (loop)
  (let ([input (read)])
    (send channel "push" input))
  (loop))

(loop)
