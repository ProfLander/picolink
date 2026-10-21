#lang racket/base

(require racket/syntax

         ee-lib

         picolink/syntax
         picolink/backend
         picolink/module/scopes)

(provide (all-defined-out))

(struct/cross-phase language [%extract
                              %wrap
                              %parse
                              %compiler])

(define (make-language #:extract  extract
                       #:wrap     wrap
                       #:parse    parse
                       #:compiler compiler)
  (language extract
            wrap
            parse
            compiler))

(define (language-ident lctx)
    (datum->syntax lctx '#%language) )

(define (syntax-local-language lctx)
  (syntax-local-eval (language-ident lctx)))

(define (language-extract lang stx)
  ((language-%extract lang) stx))

(define (language-wrap lang stx)
  ((language-%wrap lang) stx))

(define (language-parse lang
                        stx)
  (parameterize ([current-def-ctx (current-module-scope)])
    ((language-%parse lang)
     (syntax-local-introduce stx))))

(define (language-compiler lang backend)
  ((language-%compiler lang) ((backend-target backend))))
