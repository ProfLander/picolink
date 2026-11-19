#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/dict
         racket/sequence

         picolink/syntax)

(provide current-scopes
         current-scope
         make-scope
         scope-names
         scope-empty?
         in-scope
         with-scope
         with-new-scope
         bind!
         lookup)

(struct/cross-phase scope [name bindings])

(define (make-scope name)
  (scope name (box null)))

(define (scope-ref scope sym)
  (let ([binding (assoc sym (unbox (scope-bindings scope)) datum=?)])
    (and binding (cdr binding))))

(define (scope-set! scope sym binding)
  (set-box! (scope-bindings scope)
            (cons (cons sym binding)
                  (unbox (scope-bindings scope)))))

(define (scope-names scope)
  (dict-keys (unbox (scope-bindings scope))))

(define (scope-empty? scope)
  (null? (unbox (scope-bindings scope))))

(define (in-scope scope)
  (in-dict (unbox (scope-bindings scope))))

(define current-scopes
  (make-parameter null))

(define (current-scope)
  (let ([scopes (current-scopes)])

    (unless (pair? scopes)
      (error "not in a binding scope"))

    (car (current-scopes))))

(define-syntax with-scope
  (syntax-parser
    [(_ scope body ...)
     (syntax/loc this-syntax
       (parameterize ([current-scopes (cons scope (current-scopes))])
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
  (for/first ([scope (in-list (current-scopes))]
              #:do [(define binding (scope-ref scope ident))]
              #:when binding)

    binding))


(define (bind! ident binding)

  (let ([scope (current-scope)])

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
