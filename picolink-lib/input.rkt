#lang racket/base

(require racket/contract
         racket/generic
         racket/match
         racket/sequence

         syntax/parse
         syntax/id-set
         syntax/id-table

         (for-template racket/base))

(provide (all-defined-out))

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

(define input-registry
  (make-hash))

(define/contract (register-input name in)
  (-> symbol?
      (-> syntax?
          (generic-instance/c
           gen:input

           [source (-> input? any/c)]

           [collect-require
            (-> input? syntax?
                (or/c (cons/c syntax? immutable-free-id-table?) #f))]
           [collect-requires (or/c (-> input?
                                       (hash/c syntax?
                                               immutable-free-id-table?))
                                   #f)]

           [collect-provide
            (-> input? syntax? (or/c immutable-free-id-set? #f))]
           [collect-provides (or/c (-> input? free-id-set?) #f)]

           [compiler
            (-> input? symbol? (-> syntax? any/c))]
           [compile
            (or/c (-> input? symbol? any/c) #f)]))
      void)
  (hash-set! input-registry name in))

(define/contract (input name)
  (-> symbol? (-> syntax? input?))
  (hash-ref input-registry name))

(define/contract (make-input name source)
  (-> symbol? syntax? input?)
  ((input name) source))
