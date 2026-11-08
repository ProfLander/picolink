#lang picolink/r-lua/base

(require (only lua/io  read)
         
         (only lua/ext/object  send)

         (only love/ext/thread  get-channel)
         
         (only r-lua/prelude/binding    let)
         (only r-lua/prelude/definition  define))

(define channel (get-channel "stdin"))

(define (loop)
  (let ([input (read)])
    (send channel "push" input))
  (loop))

(loop)
