#lang racket/base

(require racket/syntax)

(provide (all-defined-out))

(struct backend [target
                 %search-paths
                 %transform
                 %link
                 %run
                 %interact]
  #:transparent)

(define (make-backend #:target       target
                      #:search-paths search-paths
                      #:transform    transform
                      #:link         link
                      #:run          run
                      #:interact     interact)

  (backend target
           search-paths
           transform
           link
           run
           interact))

(define (backend-ident lctx)
  (datum->syntax lctx '#%backend))

(define (syntax-local-backend lctx)
  (syntax-local-eval (backend-ident lctx)))

(define (backend-search-paths backend)
  ((backend-%search-paths backend)))

(define (backend-transform backend lctx prog)
  ((backend-%transform backend) lctx prog))

(define (backend-link backend lctx compiled)
  ((backend-%link backend) lctx compiled))

(define (backend-run backend lctx linked)
  ((backend-%run backend) lctx linked))

(define (backend-interact backend lctx compiled)
  ((backend-%interact backend) lctx compiled))
