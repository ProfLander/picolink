#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         ee-lib

         picolink/syntax
         picolink/language
         picolink/module/source
         picolink/module/compiled
         picolink/module/scopes)

(provide (all-defined-out))

(define current-module-ident
  (make-parameter #f))

(struct/cross-phase parsed-module [ident
                                   language
                                   require-scope
                                   provide-scope
                                   module-scope
                                   stx])

(define (make-parsed-module ident lang stx)
  (parsed-module ident
                 lang
                 (current-require-scope)
                 (current-provide-scope)
                 (current-def-ctx)
                 (and stx (syntax-local-introduce stx))))

(define (parse-module ident mod)

  (parameterize ([current-module-ident ident])
    (with-module-scopes

      (let ([lang (source-module-language mod)])

        (language-inject lang)

        (let* ([stx (source-module-stx mod)]
               [stx (language-parse lang stx)])

          (make-parsed-module ident lang stx))))))

(define (compile-module-syntax lang backend stx)

  (let ([compiler (language-compiler lang backend)])

    (compiler stx)))

(define (compile-parsed-module ident backend parsed)

  (with-parsed-module-scopes parsed

    (let ([lang (parsed-module-language parsed)]
          [stx  (parsed-module-stx parsed)])

      (compiled-module ident
                       lang
                       (current-require-scope)
                       (current-provide-scope)
                       (current-def-ctx)
                       (and stx (compile-module-syntax lang
                                                       backend
                                                       stx))))))

(define-syntax with-parsed-module-scopes
  (syntax-parser
    [(_ parsed body ...)
     #'(parameterize ([current-require-scope
                       (parsed-module-require-scope parsed)]
                      [current-provide-scope
                       (parsed-module-provide-scope parsed)]
                      [current-module-scopes null]
                      [current-def-ctx
                       (parsed-module-module-scope  parsed)])
         body ...)]))
