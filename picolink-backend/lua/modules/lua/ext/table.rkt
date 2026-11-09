#lang picolink/r-lua/base

(require (only lua/ext/type table?)

         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/error       error)
         (only r-lua/prelude/format      format)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define (check-table t)
  (if (table? t)
      t
      (error (format "~a is not a table" t))))

(define (table-ref t k)
  (check-table t)
  (s-lua (#%member t (quote k))))

(define table-set!
  (s-lua
   (#%function (t k v)
               (#%block
                (#%call check-table t)
                (#%assign [(#%member t (quote k))] [v])
                (#%return %void)))))

(define (table-length t)
  (check-table t)
  (s-lua (#%length t)))
