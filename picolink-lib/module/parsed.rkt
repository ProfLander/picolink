#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/syntax
         picolink/language
         picolink/scope
         picolink/module/source
         picolink/module/compiled
         picolink/module/scopes

         (only-in picolink/r-lua/anf   [current-debug debug:r-lua->anf])
         (only-in picolink/r-lua/s-lua [current-debug debug:anf->s-lua]))

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
                 (current-scope)
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

    (let* ([lang (parsed-module-language parsed)]
           [stx  (parsed-module-stx parsed)]
           [stx (and stx
                     (parameterize ([debug:r-lua->anf
                                     (equal? ident 'entry-point)]

                                    [debug:anf->s-lua
                                     (equal? ident 'entry-point)])

                       (compile-module-syntax lang backend stx)))])

      (compiled-module ident
                       lang
                       (current-require-scope)
                       (current-provide-scope)
                       (current-scope)
                       stx))))

(define-syntax with-parsed-module-scopes
  (syntax-parser
    [(_ parsed body ...)
     (syntax/loc this-syntax

       (parameterize ([current-require-scope
                       (parsed-module-require-scope parsed)]
                      [current-provide-scope
                       (parsed-module-provide-scope parsed)]
                      [current-foreign-scopes null])

         (with-scope (parsed-module-module-scope parsed)
           body ...)))]))
