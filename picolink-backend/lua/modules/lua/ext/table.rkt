#lang picolink/r-lua/base

(require (only lua/string format)

         (only lua/ext/type table?)
         (only lua/ext/object field set-field!)

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
  (field t k))

(define (table-set! t k v)
  (check-table t)
  (set-field! t k v))

(define (table-length t)
  (check-table t)
  (s-lua (#%length t)))
