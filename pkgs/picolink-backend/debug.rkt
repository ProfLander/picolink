#lang picolink/backend

#:target   current-debug-target
#:compile  debug-compile
#:run      debug-run
#:interact debug-interact

(require racket/string
         racket/pretty

         picolink/scope
         picolink/module/compiled
         picolink/module/parameters

         (for-template racket/base))

(provide (all-defined-out))

(define current-debug-target
  (make-parameter #f))

(define (debug-compile _stx)

  (define require-scope (current-require-scope))
  (define provide-scope (current-provide-scope))
  (define lexical-scope (current-lexical-scope))

  (define (print-binding ident)
    (define binding (lookup ident))
    (displayln (format "  ~a:" (syntax-e ident)))
    (displayln (format "   ~a" (string-replace (pretty-format binding)
                                               "\n" "\n   "))))

  (displayln (format "~a:" (syntax-e (current-module-ident))))

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
  (with-scope lexical-scope
    (for ([ident (in-list (scope-names lexical-scope))])
      (print-binding ident))))

(define (debug-run _lctx _linked)
  #'(void))

(define (debug-interact _lctx _stx)
  #'(void))
