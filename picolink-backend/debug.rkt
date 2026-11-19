#lang picolink/backend

#:target   current-debug-target
#:link     debug-link
#:run      debug-run
#:interact debug-interact

(require racket/string
         racket/pretty

         picolink/scope
         picolink/module/scopes
         picolink/module/compiled

         (for-template racket/base))

(provide (all-defined-out))

(define current-debug-target
  (make-parameter #f))

(define (debug-link _lctx prog)

  (for ([mod (in-list prog)])

    (define ident         (compiled-module-ident mod))
    (define require-scope (compiled-module-require-scope mod))
    (define provide-scope (compiled-module-provide-scope mod))
    (define module-scope  (compiled-module-module-scope mod))

    (define (print-binding ident)
      (define binding (lookup ident))
      (displayln (format "  ~a:" (syntax-e ident)))
      (displayln (format "   ~a" (string-replace (pretty-format binding)
                                                 "\n" "\n   "))))

    (displayln (format "~a:" (syntax-e ident)))

    (displayln " requires:")
    (with-scope require-scope
      (for ([ident (in-list (scope-names require-scope))])
        (print-binding ident)))

    (displayln "")

    (displayln " provides:")
    (with-scope provide-scope
      (for ([ident (in-list (scope-names provide-scope))])
        (print-binding ident)))

    (displayln "")

    (displayln " defines:")
    (with-scope module-scope
      (for ([ident (in-list (scope-names module-scope))])
        (print-binding ident)))))

(define (debug-run _lctx _linked)
  #'(void))

(define (debug-interact _lctx _stx)
  #'(void))
