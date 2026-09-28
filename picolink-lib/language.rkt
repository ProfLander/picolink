#lang racket/base

(require (for-syntax racket/base
                     racket/contract
                     syntax/modresolve)

         racket/generic
         racket/path

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
                  [language language]
                  [config-id (syntax-local-identifier-as-binding
                              (datum->syntax lctx 'config))]
                  [compile-id (syntax-local-identifier-as-binding
                               (datum->syntax lctx 'compile))]
                  [link-id (syntax-local-identifier-as-binding
                            (datum->syntax lctx 'link))]

                  [run-id (syntax-local-identifier-as-binding
                           (datum->syntax lctx 'run))])

      #'(#%plain-module-begin
         (require picolink)

         (provide language-id compile-id)

         (define language-id language)

         (define (config-id)
           (define module-path
             (path-replace-extension
              (variable-reference->module-source (#%variable-reference))
              ""))

           (define config (find-config module-path))
           (unless config
             (error "failed to find config.rkt"))

           (define entry-point
             (find-relative-path (config-root config) module-path))

           (config-update (find-config module-path)
                          #:entry-point entry-point))

         (define (compile-id)
           (compile (config-id)))

         (define (link-id)
           (link (config-id)))

         (define (run-id)
           (run (config-id)))

         (module+ main
           (run-id))))))
