#lang racket/base

(require picolink/backend/generic)

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

(define (make-racket)
  (racket))
