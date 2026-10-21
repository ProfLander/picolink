#lang racket/base

(require picolink/program
         picolink/program/syntax
         picolink/program/scope
         picolink/backend
         picolink/language
         picolink/module)

(provide (all-defined-out))

(define (#%program-interaction lctx stx)
  (define lang        (syntax-local-language lctx))
  (define backend     (syntax-local-backend lctx))
  (define program     (syntax-local-program lctx))

  (parameterize ([current-program-scope program])

    (define entry-point (lookup-in-program #'entry-point))

    (with-parsed-module-scopes entry-point

      (let* ([stx (language-wrap lang #`(#,stx))]
             [stx (language-parse lang stx)]
             [stx (compile-module-syntax lang backend stx)])

        (backend-interact backend lctx stx)))))
