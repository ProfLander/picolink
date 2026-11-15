#lang picolink/r-lua/base

(require (only lua/string format gsub)

         (only r-lua/prelude/binding     let*)
         (only r-lua/prelude/definition  define))

(provide (rename format* format))

(define (format* str . [: vs #%values])
  (let* ([str (gsub str "%%" "%%%%")]
         [str (gsub str "~a" "%%s")])
    (format str vs)))
