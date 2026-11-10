#lang picolink/r-lua/base

(require (only lua/ext/metatable  get-metatable set-metatable!)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/function    const)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       and)
         (only r-lua/prelude/table       table)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

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
           (set-metatable! (table)
                           (table [__tostring (const name-str)])))

         (define (predicate-name v)
           (and v
                (let ([mt (get-metatable v)])
                  (s-lua (#%eq mt name))))))))])

(define (ascribe type value)
  (set-metatable! value type))

(define-type Boolean)
(define-type Number)
(define-type String)
(define-type Procedure)
