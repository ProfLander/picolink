#lang picolink/r-lua/base

(require (only lua/io  read)
         
         (only lua/ext/ops  :)

         (only love/ext/thread  get-channel)
         
         (only r-lua/prelude/binding    let)
         (only r-lua/prelude/definition  define))

(define channel (get-channel "stdin"))

(let loop ()
  (let ([input (read)])
    (: channel "push" input))
  (loop))
