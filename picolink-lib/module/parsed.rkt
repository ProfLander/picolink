#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         ee-lib

         picolink/syntax
         picolink/language
         picolink/module/source
         picolink/module/compiled
         picolink/module/parameters)

(provide (all-defined-out))

(struct/cross-phase parsed-module [ident
                                   language
                                   require-scope
                                   module-scope
                                   provide-scope
                                   stx])

(define (make-parsed-module ident lang stx)
  (parsed-module ident
                 lang
                 (current-require-scope)
                 (current-module-scope)
                 (current-provide-scope)
                 (and stx (syntax-local-introduce stx))))

(define (parse-module program ident mod)

  (with-module-scopes

    (let* ([lang (source-module-language mod)]
           [stx  (source-module-stx mod)]
           [stx  (language-parse lang program stx)])

      (make-parsed-module ident lang stx))))

(define (compile-module-syntax lang backend stx)

  (let ([compiler (language-compiler lang backend)])

    (parameterize ([current-def-ctx (current-module-scope)])

      (compiler stx))))

(define (compile-parsed-module ident backend parsed)

  (with-parsed-module-scopes parsed

    (let ([lang (parsed-module-language parsed)]
          [stx  (parsed-module-stx parsed)])

      (compiled-module ident
                       lang
                       (current-require-scope)
                       (current-module-scope)
                       (current-provide-scope)
                       (and stx (compile-module-syntax lang
                                                       backend
                                                       stx))))))

(define-syntax with-parsed-module-scopes
  (syntax-parser
    [(_ parsed body ...)
     #'(parameterize ([current-require-scope
                       (parsed-module-require-scope parsed)]
                      [current-module-scope
                       (parsed-module-module-scope  parsed)]
                      [current-provide-scope
                       (parsed-module-provide-scope parsed)])
         body ...)]))
