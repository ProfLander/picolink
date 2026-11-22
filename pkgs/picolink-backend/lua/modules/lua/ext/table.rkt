#lang picolink/r-lua/base

(require (only lua/string format)

         (only lua/ext/ops    -> = length)
         (only lua/ext/type   table?)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/error       error)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define (array-table . [: vs #%values])
  (s-lua (#%table vs)))

(define (check-table t)
  (if (table? t)
      t
      (error (format "%s is not a table" t))))

(define (table-ref t k)
  (check-table t)
  (-> t k))

(define (table-set! t k v)
  (check-table t)
  (= t k v))

(define (table-length t)
  (check-table t)
  (length t))
