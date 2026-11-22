#lang racket/base

(require picolink/syntax)

(provide (all-defined-out))

(struct/cross-phase backend [target
                             %search-paths
                             %modules
                             %compile
                             %run
                             %interact])

(define (make-backend #:target       target
                      #:search-paths search-paths
                      #:modules      modules
                      #:compile      compile
                      #:run          run
                      #:interact     interact)

  (backend target
           search-paths
           modules
           compile
           run
           interact))

(define (backend-search-paths backend)
  ((backend-%search-paths backend)))

(define (backend-modules backend)
  ((backend-%modules backend)))

(define (backend-compile backend stx)
  ((backend-%compile backend) stx))

(define (backend-run backend lctx linked)
  ((backend-%run backend) lctx linked))

(define (backend-interact backend lctx compiled)
  ((backend-%interact backend) lctx compiled))
