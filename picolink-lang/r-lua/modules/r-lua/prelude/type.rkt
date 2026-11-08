#lang picolink/r-lua/base

(require (only lua/ext/metatable
               get-metatable
               set-metatable!)

         r-lua/prelude/binding
         r-lua/prelude/definition
         r-lua/prelude/equality
         r-lua/prelude/function
         r-lua/prelude/interface
         r-lua/prelude/lambda
         r-lua/prelude/logic
         r-lua/prelude/string
         r-lua/prelude/s-lua
         r-lua/prelude/print)

(provide (all-defined))

(define Type
  (set-metatable!
   (s-lua (#%table))
   (s-lua (#%table ["__tostring"
                    (#%function ()
                                (#%block
                                 (#%return "#<Type>")))]))))

(define (Type? v)
  (eq? (get-metatable v) Type))

(impl ToString Type
      (let ([to-string (const "Type")])
        (s-lua (#%table ["to-string" to-string]))))

(define-macro define-type
  [(_ name
      (~or (~optional (~seq #:printed-name printed-name)
                      #:defaults ([printed-name #'name]))
           (~optional (~seq #:predicate-name predicate-name)
                      #:defaults ([predicate-name
                                   (format-id #'name "~a?" #'name)])))
      ...)

   (with-syntax ([name-str (symbol->string (syntax-e #'printed-name))])
     (syntax/loc this-syntax
       (begin
         (define name
           (set-metatable! (s-lua (#%table))
                           Type))

         (define (predicate-name v)
           (and v
                (eq? (get-metatable v) name)))

         (impl ToString name
               (let ([to-string (λ (self)
                                  name-str)])
                 (s-lua (#%table ["to-string" to-string])))))))])

(define (ascribe type value)
  (set-metatable! value type))
