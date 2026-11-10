#lang picolink/r-lua/base

(require (rename lua/string format %format)

         (only lua/ext/object  send)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define))

(provide (all-defined))

(define (format str . [: vs #%values])
  (let ([str (send str "gsub" "~a" "%%s")])
    (%format str vs)))
