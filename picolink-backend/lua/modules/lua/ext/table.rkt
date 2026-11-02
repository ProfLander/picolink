#lang picolink/r-lua/base

(require (only lua error)
         (only lua/string format)
         (only lua/ext/type table?)

         r-lua/prelude/conditional
         r-lua/prelude/surface
         r-lua/prelude/void)

(provide (all-defined))

(define (check-table t)
  (unless (table? t)
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
