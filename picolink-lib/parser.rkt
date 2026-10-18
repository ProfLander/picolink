#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/syntax

         syntax/strip-context

         picolink/ee-lib
         picolink/syntax
         picolink/module/parsed
         picolink/module/import
         picolink/module/export)

(provide (all-defined-out))

(define current-debug
  (make-parameter #t))

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

(define (require! lctx src-mod-path)

  (when (current-debug)
    (displayln "require!")
    (println src-mod-path))

  (require/prefix! lctx #f src-mod-path))

(define (require/prefix! lctx prefix src-mod-path)

  (when (current-debug)
    (displayln "require/prefix!")
    (println prefix)
    (println src-mod-path))

  (require/prefix-all-except! lctx prefix src-mod-path null))

(define (require/all-except! lctx src-mod-path exceptions)

  (when (current-debug)
    (displayln "require/all-except!")
    (println src-mod-path)
    (println exceptions))

  (require/prefix-all-except! lctx #f src-mod-path exceptions))

(define (require/prefix-all-except! lctx prefix src-mod-path exceptions)

  (when (current-debug)
    (displayln "require/prefix-all-except!")
    (println prefix)
    (println src-mod-path)
    (println exceptions))

  (define mod
    (parameterize ([current-def-ctx (current-program-scope)])
      (lookup src-mod-path parsed-module?)))

  (displayln "mod:")
  (println mod)

  (unless mod
    (raise-syntax-error
     #f
     "failed to resolve module"
     lctx
     src-mod-path))

  (define provide-scope (parsed-module-provide-scope mod))

  (define provides (scope-idents provide-scope))

  (println provides)

  (for ([prov (in-list provides)]
        #:when (not (member prov exceptions datum=?)))

    (define local-id
      (datum->syntax src-mod-path
                     (syntax-e prov)
                     src-mod-path
                     src-mod-path))

    (define prefixed
      (and prefix
           (format-id local-id "~a~a" prefix local-id
                      #:source local-id
                      #:props local-id)))

    (require/rename! lctx src-mod-path local-id (or prefixed local-id))))

(define (require/only! lctx src-mod-path local-ids)

  (when (current-debug)
    (displayln "require/only!")
    (println src-mod-path)
    (println local-ids))

  (for ([local-id (in-list local-ids)])
    (require/rename! lctx src-mod-path local-id local-id)))

(define (require/rename! lctx
                         src-mod-path
                         src-id
                         local-id)

  (when (current-debug)
    (displayln "require/rename!")
    (println src-mod-path)
    (println src-id)
    (println local-id))

  (define src-sym (syntax-e src-id))

  (define mod
    (parameterize ([current-def-ctx (current-program-scope)])
      (lookup src-mod-path parsed-module?)))

  (unless mod
    (raise-syntax-error
     #f
     "failed to resolve module"
     lctx
     src-mod-path))

  (define prov
    (parameterize ([current-def-ctx (parsed-module-provide-scope mod)])
      (lookup (datum->syntax #f src-sym))))

  (define local-id* (syntax-local-introduce local-id))

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
    (parameterize ([current-def-ctx (parsed-module-module-scope mod)])
      (lookup (export-local-id prov))))

  (bind! local-id binding))

(define (provide! lctx local-id)

  (when (current-debug)
    (displayln "provide!")
    (println local-id))

  (provide/rename! lctx local-id local-id))

(define (provide/all-from! lctx mod-id)

  (when (current-debug)
    (displayln "provide/all-from!")
    (println mod-id))

  (provide/all-from-except! lctx mod-id null))

(define (provide/all-from-except! lctx mod-id exceptions)

  (when (current-debug)
    (displayln "provide/all-from-except!")
    (println mod-id)
    (println exceptions))

  (define require-scope (current-require-scope))

  (define requires
    (filter (λ (def)

              (define import
                (parameterize ([current-def-ctx require-scope])
                  (lookup def)))

              (define import-mod (import-src-mod-path import))

              (and (datum=? mod-id import-mod)
                   (not (member def exceptions
                                datum=?))))

            (scope-idents require-scope)))

  (for ([req (in-list requires)])
    (provide! lctx req)))

(define (provide/all-defined! lctx)

  (when (current-debug)
    (displayln "provide/all-defined!"))

  (provide/all-defined-except! lctx null))

(define (provide/all-defined-except! lctx exceptions)

  (when (current-debug)
    (displayln "provide/all-defined-except!")
    (println exceptions))

  (provide/prefix-all-defined-except! lctx #f exceptions))

(define (provide/prefix-all-defined! lctx prefix)

  (when (current-debug)
    (displayln "provide/prefix-all-defined!")
    (println prefix))

  (provide/prefix-all-defined-except! lctx prefix null))

(define (provide/prefix-all-defined-except! lctx prefix exceptions)

  (when (current-debug)
    (displayln "provide/prefix-all-defined-except!")
    (println prefix)
    (println exceptions))

  (define module-scope (current-module-scope))
  (define require-scope (current-require-scope))

  (define requires (scope-idents require-scope))

  (define definitions (filter (λ (def)
                                (and (not (member def requires
                                                  datum=?))
                                     (not (member def exceptions
                                                  datum=?))))
                              (scope-idents module-scope)))

  (for ([definition (in-list definitions)])

    (define prefixed
      (and prefix
           (format-id definition "~a~a" prefix definition
                      #:source definition
                      #:props definition)))

    (provide/rename! lctx definition (or prefixed definition))))

(define (provide/rename! lctx
                         local-id
                         out-id)

  (when (current-debug)
    (displayln "provide/rename!")
    (println local-id)
    (println out-id))

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
