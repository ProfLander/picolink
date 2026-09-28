#lang racket/base

(require racket/contract
         racket/struct

         picolink/backend/link-ctx)

(provide (all-defined-out))

(struct run-ctx (link-ctx artifact)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self)
        'run-ctx)
      (λ (self)
        (list (run-ctx-link-ctx self)
              (cons 'artifact
                    (run-ctx-artifact self))))))])

(define/contract (make-run-ctx link-ctx artifact)
  (-> link-ctx?
      any/c
      run-ctx?)
  (run-ctx link-ctx artifact))

(define (run-ctx-project ctx)
  (link-ctx-project (run-ctx-link-ctx ctx)))

(define (run-ctx-root ctx)
  (link-ctx-root (run-ctx-link-ctx ctx)))

(define (run-ctx-entry-point ctx)
  (link-ctx-entry-point (run-ctx-link-ctx ctx)))

(define (run-ctx-backend ctx)
  (link-ctx-backend (run-ctx-link-ctx ctx)))

(define (run-ctx-build-directory ctx)
  (link-ctx-build-directory (run-ctx-link-ctx ctx)))

(define (run-ctx-search-paths ctx)
  (link-ctx-search-paths (run-ctx-link-ctx ctx)))

(define (run-ctx-requires ctx)
  (link-ctx-requires (run-ctx-link-ctx ctx)))

(define (run-ctx-provides ctx)
  (link-ctx-provides (run-ctx-link-ctx ctx)))

(define (run-ctx-modules ctx)
  (link-ctx-modules (run-ctx-link-ctx ctx)))
