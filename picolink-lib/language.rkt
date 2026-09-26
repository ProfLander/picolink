#lang racket/base

(require (for-syntax racket/base
                     racket/contract)

         racket/generic
         racket/sequence
         racket/set

         syntax/parse

         (for-template racket/base)

         picolink/language/requires
         picolink/language/provides)

(provide (all-from-out picolink/language/requires)
         (all-from-out picolink/language/provides)
         (all-defined-out)
         (for-syntax (all-defined-out)))

(define-generics language
  (requires language)
  (provides language)
  (source language)

  (compiler language name)
  (compile language name)

  #:fallbacks
  [(define/generic call-source source)
   (define/generic call-requires requires)
   (define/generic call-provides provides)
   (define/generic call-compiler compiler)

   (define (search-paths _language)
     null)

   (define (requires language)
     (make-module-requires))

   (define (provides language)
     (make-module-provides))

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
