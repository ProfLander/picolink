#lang racket/base

(require racket/contract

         syntax/parse

         picolink/backend
         picopass/lang/s-lua/to-source)

(provide (all-defined-out))

(struct backend-lua ()
  #:transparent
  #:methods gen:backend

  [(define (output-name self)
     's-lua)

   (define (link self outputs)
     (displayln "outputs:")
     (println outputs)
     (error "unimplemented:" 'link))

   (define (run self linked)
     (error "unimplemented:" 'run))])

(define/contract (make-backend-lua)
  (-> backend-lua?)
  (backend-lua))

(register-backend 'lua make-backend-lua)
