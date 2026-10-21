#lang picolink/backend

#:target   current-debug-target
#:link     debug-link
#:run      debug-run
#:interact debug-interact

(require racket/string
         racket/pretty

         picolink/ee-lib
         picolink/module/compiled
         picolink/module/scopes

         (for-template racket/base))

(provide (all-defined-out))

(define current-debug-target
  (make-parameter #f))

(define (debug-link _lctx prog)

  (for ([mod (in-list prog)])

    (define ident         (compiled-module-ident mod))
    (define require-scope (compiled-module-require-scope mod))
    (define module-scope  (compiled-module-module-scope mod))
    (define provide-scope (compiled-module-provide-scope mod))

    (define (print-binding ident)
      (define binding (lookup-in-module ident))
      (displayln (format "  ~a:" (syntax-e ident)))
      (displayln (format "   ~a" (string-replace (pretty-format binding)
                                                 "\n" "\n   "))))

    (displayln (format "~a:" (syntax-e ident)))

    (displayln " requires:")
    (parameterize ([current-def-ctx require-scope])
      (for ([ident (in-list (scope-idents require-scope))])
        (print-binding ident)))

    (displayln "")

    (displayln " defines:")
    (parameterize ([current-def-ctx module-scope])
      (for ([ident (in-list (scope-idents module-scope))])
        (print-binding ident)))

    (displayln "")

    (displayln " provides:")
    (parameterize ([current-def-ctx provide-scope])
      (for ([ident (in-list (scope-idents provide-scope))])
        (print-binding ident)))))

(define (debug-run _lctx _linked)
  #'(void))

(define (debug-interact)
  #'(void))
