#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/alist
         picolink/syntax
         picolink/module/parameters)

(provide current-lexical-scopes
         current-lexical-scope
         make-scope
         scope-names
         scope-empty?
         in-scope
         with-scope
         with-new-scope
         with-module-scopes
         bind!
         lookup
         lookup-foreign)

(struct/cross-phase scope [name bindings])

(define (make-scope name)
  (scope name (make-alist)))

(define (scope-ref scope key)
  (alist-ref (scope-bindings scope) key))

(define (scope-set! scope key val)
  (alist-set! (scope-bindings scope) key val))

(define (scope-names scope)
  (alist-keys (scope-bindings scope)))

(define (scope-empty? scope)
  (alist-empty? (scope-bindings scope)))

(define (in-scope scope)
  (in-alist (scope-bindings scope)))

(define (current-lexical-scope)
  (let ([scopes (current-lexical-scopes)])

    (unless (pair? scopes)
      (error "not in a binding scope"))

    (car (current-lexical-scopes))))

(define-syntax with-scope
  (syntax-parser
    [(_ scope body ...)
     (syntax/loc this-syntax
       (parameterize ([current-lexical-scopes
                       (cons scope (current-lexical-scopes))])
         body ...))]))

(define-syntax with-new-scope
  (syntax-parser
    [(_ name body ...)
     (syntax/loc this-syntax
       (let ([name (make-scope 'name)])
         (with-scope name
           body ...)))]))

(define (%normalize-key key)
  (cond
    [(identifier? key) (syntax-e key)]
    [(symbol? key) key]
    [else
     (raise
      (exn:fail:contract
       (format "bind!: invalid key ~a" key)
       (current-continuation-marks)))]))

(define (lookup ident)
  (for/first ([scope (in-list (current-lexical-scopes))]
              #:do [(define binding (scope-ref scope ident))]
              #:when binding)

    binding))


(define (bind! ident binding)

  (let ([scope (current-lexical-scope)])

    (when (scope-ref scope ident)
      (error (scope-name scope)
             "cannot rebind identifier ~a"
             (syntax-e ident)))

    (scope-set! scope ident binding)

    ident))

(module+ test
  (with-new-scope outer
    (bind! #'a 1234)
    (with-new-scope inner
      (bind! #'a 5678)
      (bind! #'b 5678)
      (println (lookup #'a)))))

(define (lookup-foreign ident)

  "Lookup IDENT in the current module scope"

  (for/first ([pair (in-list (current-foreign-scopes))]
              #:do [(define source-module (car pair))
                    (define scope (cdr pair))

                    (define bind
                      (with-scope scope
                        (lookup ident)))]

              #:when bind)

    (cons source-module bind)))

(define-syntax with-module-scopes
  (syntax-parser
    [(_ body ...)
     (syntax/loc this-syntax

       (parameterize ([current-require-scope (make-scope 'require)]
                      [current-provide-scope (make-scope 'provide)]
                      [current-foreign-scopes null])

         (with-new-scope lexical
           body ...)))]))
