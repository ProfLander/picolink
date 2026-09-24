#lang racket/base

(require (for-syntax racket/base
                     racket/contract)

         racket/generic
         racket/sequence
         racket/set

         syntax/parse

         (for-template racket/base))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-generics language
  (source language)

  (collect-require language stx)
  (collect-requires language)

  (collect-provide language stx)
  (collect-provides language)

  (compiler language name)
  (compile language name)

  #:fallbacks
  [(define/generic call-source source)
   (define/generic call-collect-require collect-require)
   (define/generic call-collect-provide collect-provide)
   (define/generic call-compiler compiler)

   (define (search-paths _language)
     null)

   (define (collect-require _language _stx)
     #f)

   (define (collect-requires language)
     (for/hash ([stx (in-syntax (call-source language))]
                #:do [(define req (call-collect-require language stx))]
                #:when req)
       (values (car req) (cdr req))))

   (define (collect-provide _language _stx)
     #f)

   (define (collect-provides language)
     (for/fold ([acc (set)])
               ([stx (in-syntax (call-source language))])
       (syntax-parse stx
         [stx
          #:do [(define res (call-collect-provide language #'stx))]
          #:when res
          (set-union acc res)]
         [_
          acc])))

   (define (compile language name)
     ((call-compiler language name) (call-source language)))])

(begin-for-syntax
  (define/contract (make-language-module lctx language)
    (-> syntax? syntax? syntax?)

    (with-syntax ([language-id (syntax-local-identifier-as-binding
                             (datum->syntax lctx 'language))]
                  [language language])

      #'(#%plain-module-begin
         (provide language-id)
         (define language-id language)))))
