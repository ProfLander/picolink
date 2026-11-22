#lang racket/base

(require picolink/program/syntax
         picolink/scope
         picolink/program/scope
         picolink/backend
         picolink/backend/parameters
         picolink/language
         picolink/module)

(provide (all-defined-out))

(define (#%program-interaction lctx stx)
  (define backend     (current-backend))
  (define lang        (syntax-local-language lctx))
  (define program     (syntax-local-program lctx))

  (parameterize ([current-program-scope program])

    (define entry-point
      (with-scope program
        (lookup #'entry-point)))

    (with-picolink-module-scopes entry-point

      (let* ([stx (language-wrap lang #`(#,stx))]
             [stx (language-compile lang backend stx)])

        (backend-interact backend lctx stx)))))
