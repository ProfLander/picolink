#lang racket/base

(require (for-syntax racket/base
                     picolink/language
                     picolink/module/source))

(provide (for-syntax (all-defined-out)))

(begin-for-syntax

  (define (make-source-module-syntax lctx stx)
    
    (let* ([lang (syntax-local-language lctx)]
           [stx  (language-wrap lang stx)]
           [deps (language-extract lang stx)])

      (with-syntax ([#%language (language-ident lctx)]
                    [(dep ...) deps]
                    [stx stx])

        #'(source-module #%language
                         (list (quote-syntax dep) ...)
                         (quote-syntax stx))))))
