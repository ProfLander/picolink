#lang picolink/r-lua/base

(require (only lua/debug   traceback)
         (only lua/io      stderr)
         (only lua/string  gsub)

         (only lua/ext/ops  + .. :)

         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/function             const)
         (only r-lua/prelude/logic                or)
         (only r-lua/prelude/interface/to-string  to-string))

(provide error-handler)

(define (error-printer msg layer)
  (: stderr "write"
     (gsub
      (traceback (.. "Error: " msg)
                 (or (+ 1 layer) 1))
      "\n[^\n]+$"
      "")))

(define (error-handler msg)
  (error-printer (to-string msg) 2)
  (const 1))
