#lang picolink/r-lua/base

(require (only lua error)
         (only lua/string format)
         (only lua/ext/type table?)

         r-lua/prelude/definition
         r-lua/prelude/if
         r-lua/prelude/s-lua)

(provide (all-defined))

(define (check-table t)
  (if (table? t)
      t
      (error (format "%s is not a table" t))))

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
