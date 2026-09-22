#lang racket/base

(require racket/contract
         picolink/output
         picopass/lang/s-lua/to-source)

(provide (all-defined-out))

(struct output/s-lua (source)
  #:transparent
  #:methods gen:output
  [(define (source self)
     (output/s-lua-source self))

   (define (compile self)
     (s-lua->lua (source self)))])

(define/contract (make-output/s-lua stx)
  (-> syntax? output/s-lua?)
  (output/s-lua stx))

(register-output 's-lua make-output/s-lua)
