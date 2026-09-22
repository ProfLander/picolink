#lang racket/base

(require racket/contract
         picolink/backend)

(provide (all-defined-out))

(struct backend-racket ()
  #:transparent
  #:methods gen:backend

  [(define (output-name self)
     'racket)

   (define (link self output)
     (error "unimplemented:" 'link))

   (define (run self linked)
     (error "unimplemented:" 'run))])

(define/contract (make-backend-racket)
  (-> backend-racket?)
  (backend-racket))

(register-backend 'racket make-backend-racket)
