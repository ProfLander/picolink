#lang racket/base

(require (for-syntax racket/base
                     racket/contract)

         racket/contract
         racket/generic
         racket/match
         racket/sequence

         syntax/parse
         syntax/id-set
         syntax/id-table

         (for-template racket/base))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-generics input
  (source input)

  (collect-require input stx)
  (collect-requires input)

  (collect-provide input stx)
  (collect-provides input)

  (compiler input name)
  (compile input name)

  #:fallbacks
  [(define/generic call-source source)
   (define/generic call-collect-require collect-require)
   (define/generic call-collect-provide collect-provide)
   (define/generic call-compiler compiler)

   (define (collect-requires input)
     (for/hash ([stx (in-syntax (call-source input))]
                #:do [(define req (call-collect-require input stx))]
                #:when req)
       (values (car req) (cdr req))))

   (define (collect-provides input)
     (for/fold ([acc (immutable-free-id-set)])
               ([stx (in-syntax (call-source input))])
       (syntax-parse stx
         [stx
          #:do [(define res (call-collect-provide input #'stx))]
          #:when res
          (free-id-set-union acc res)]
         [_
          acc])))

   (define (compile input name)
     ((call-compiler input name) (call-source input)))])

(begin-for-syntax
  (define/contract (make-input-module lctx input)
    (-> syntax? syntax? syntax?)

    (with-syntax ([input-id (syntax-local-identifier-as-binding
                             (datum->syntax lctx 'input))]
                  [input input])

      #'(#%plain-module-begin
         (provide input-id)
         (define input-id input)))))
