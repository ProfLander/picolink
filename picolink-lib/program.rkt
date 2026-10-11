#lang racket/base

(require racket/function
         racket/sequence
         syntax/strip-context
         picolink/module)

(require picolink/ee-lib)

(provide (all-defined-out))

(define (program-module program ident)
  (parameterize ([current-def-ctx program])
    (lookup ident)))

(define (program-bind! program ident stx)
  (parameterize ([current-def-ctx program])
    (bind! ident stx)))

(define program-module-identifiers scope-idents)

(define (program-ident lctx)
  (datum->syntax lctx '#%program))

(define (syntax-local-program lctx)
  (syntax-local-value (program-ident lctx)))


(define (parse-program mod-idents)

  (with-scope program

    (for ([mod-ident (in-list mod-idents)])

      (let* ([ident  (syntax-local-introduce (strip-context mod-ident))]
             [mod    (syntax-local-module mod-ident)]
             [parsed (parse-module program mod)])

        (program-bind! program ident parsed)))

    program))

(define (parallel-compile-program program backend)

  (let* ([mod-ids (program-module-identifiers program)]
         [pool    (make-parallel-thread-pool)]

         [threads
          (for/list ([mod-id (in-list mod-ids)])

            (define mod (program-module program mod-id))

            (thread (thunk (compile-parsed-module mod-id
                                                  backend
                                                  mod))
                    #:pool pool
                    #:keep 'results))])

    (map thread-wait threads)))
