#lang racket/base

(require racket/syntax

         picolink/syntax
         picolink/backend)

(provide (all-defined-out))

(struct/cross-phase language [%search-paths
                              %modules
                              %extract
                              %wrap
                              %inject
                              %compiler])

(define (make-language #:search-paths search-paths
                       #:modules      modules
                       #:extract      extract
                       #:wrap         wrap
                       #:inject       inject
                       #:compiler     compiler)
  (language search-paths
            modules
            extract
            wrap
            inject
            compiler))

(define (language-ident lctx)
    (datum->syntax lctx '#%language) )

(define (syntax-local-language lctx)
  (syntax-local-eval (language-ident lctx)))

(define (language-search-paths lang)
  ((language-%search-paths lang)))

(define (language-modules lang)
  ((language-%modules lang)))

(define (language-extract lang stx)
  ((language-%extract lang) stx))

(define (language-wrap lang stx)
  ((language-%wrap lang) stx))

(define (language-inject lang)
  ((language-%inject lang)))

(define (language-compiler lang backend)
  ((language-%compiler lang) ((backend-target backend))))

(define (language-compile lang backend stx)
  ((language-compiler lang backend) stx))
