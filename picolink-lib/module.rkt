#lang racket/base

(require racket/syntax

         ee-lib

         picolink/parser
         picolink/language

         picolink/module/import
         picolink/module/export
         picolink/module/source
         picolink/module/parsed
         picolink/module/compiled)

(provide (all-from-out picolink/module/import
                       picolink/module/export
                       picolink/module/source
                       picolink/module/parsed
                       picolink/module/compiled)

         (all-defined-out))

(define (module-ident lctx [mod-ident lctx])
  (format-id lctx "~a:#%module" mod-ident))

(define (syntax-local-module lctx [mod-ident lctx])
  (syntax-local-eval (module-ident lctx mod-ident)))

(define (make-parsed-module lang stx)
  (parsed-module lang
                 (current-require-scope)
                 (current-module-scope)
                 (current-provide-scope)
                 (and stx (syntax-local-introduce stx))))

(define (parse-module program mod)

  (with-module-scopes

    (let* ([lang (source-module-language mod)]
           [stx  (source-module-stx mod)]
           [stx  (language-parse lang program stx)])

      (make-parsed-module lang stx))))

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
