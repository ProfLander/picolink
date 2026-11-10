#lang picolink/r-lua/base

(require (only lua/ext/metatable  get-metatable set-metatable!)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/equality    equal?)
         (only r-lua/prelude/function    const)
         (only r-lua/prelude/interface   impl)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       and)
         (only r-lua/prelude/print       print)
         (only r-lua/prelude/string      ToString to-string)
         (only r-lua/prelude/table       table))

(provide (all-defined))

(define Type
  (set-metatable!
   (table)
   (table [__tostring (const "#<Type>")])))

(define (Type? v)
  (equal? (get-metatable v) Type))

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
                           Type))

         (define (predicate-name v)
           (and v
                (equal? (get-metatable v) name)))

         (impl ToString name
               (define to-string (const name-str))))))])

(define (ascribe type value)
  (set-metatable! value type))
