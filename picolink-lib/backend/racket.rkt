#lang racket/base

(require racket/contract
         picolink/backend)

(provide (all-defined-out))

(struct racket ()
  #:transparent
  #:methods gen:backend

  [(define (output-name self)
     'racket)

   (define (link self ctx)
     (error "unimplemented:" 'link))

   (define (run self linked)
     (error "unimplemented:" 'run))])

(define backend-inst (racket))
