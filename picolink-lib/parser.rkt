#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         syntax/strip-context
         ee-lib
         picolink/module/parsed
         picolink/module/import
         picolink/module/export)

(provide (all-defined-out))

(define current-program-scope
  (make-parameter #f))

(define current-require-scope
  (make-parameter #f))

(define current-module-scope
  (make-parameter #f))

(define current-provide-scope
  (make-parameter #f))

(define-syntax with-module-scopes
  (syntax-parser
    [(_ body ...)
     #'(with-scope require-scope
         (with-scope module-scope
           (with-scope provide-scope
             (parameterize ([current-require-scope require-scope]
                            [current-module-scope  module-scope]
                            [current-provide-scope provide-scope])
               body ...))))]))

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

(define (require! lctx
                  src-mod-path
                  local-id
                  [src-sym (syntax-e local-id)])

  (define local-id* (syntax-local-introduce local-id))

  (define mod*
    (parameterize ([current-def-ctx (current-program-scope)])
      (lookup src-mod-path parsed-module?)))

  (unless mod*
    (raise-syntax-error
     #f
     "failed to resolve module"
     lctx
     src-mod-path))

  (define in-id*
    (strip-context local-id*))

  (define prov
    (parameterize ([current-def-ctx (parsed-module-provide-scope mod*)])
      (lookup in-id*)))

  (unless prov
    (raise-syntax-error
     #f
     (format "~a does not provide ~a"
             (syntax-e src-mod-path)
             (syntax-e local-id*))
     lctx
     local-id*))

  (parameterize ([current-def-ctx (current-require-scope)])
    (bind! local-id*
           (import local-id*
                   src-sym
                   src-mod-path
                   lctx)))

  (define binding
    (parameterize ([current-def-ctx (parsed-module-module-scope mod*)])
      (lookup (export-local-id prov))))

  (bind! local-id binding))

(define (provide! lctx
                  local-id
                  [out-id local-id])

  (define binding
    (parameterize ([current-def-ctx (current-module-scope)])
      (lookup local-id)))

  (unless binding
    (raise-syntax-error
     #f
     "unbound identifier"
     lctx
     local-id))

  (define out-id*
    (syntax-local-introduce (strip-context out-id)))

  (define provide-id
    (export local-id
            out-id*
            lctx))

  (parameterize ([current-def-ctx (current-provide-scope)])
    (bind! out-id* provide-id))

  local-id)
