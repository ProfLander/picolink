#lang picolink/r-lua/base

(require (only lua  unpack)

         (only lua/string format gsub)

         (only lua/ext/math +)

         (only lua/ext/table
               array-table
               table-length
               table-ref
               table-set!)

         (only r-lua/prelude/for                  for)
         (only r-lua/prelude/binding              let*)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/interface/to-string  to-string)
         (only r-lua/prelude/sequence             in-range))

(provide (rename format* format))

(define (format* str . [: vs array-table])

  (let* ([str   (gsub str "%%" "%%%%")]
         [str   (gsub str "~a" "%%s")]
         [count (table-length vs)])

    (for ([i (in-range 1 (+ count 1))])
      (table-set! vs i (to-string (table-ref vs i))))

    (format str (unpack vs))))
