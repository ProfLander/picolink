#lang picolink/r-lua/base

(require (only lua         get-metatable set-metatable!)
         (only lua/string  format)

         (only lua/ext/ops  =)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/condition   unless)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/error       error)
         (only r-lua/prelude/function    const)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       and)
         (only r-lua/prelude/table       table))

(provide (all-defined))

(define Type
  (set-metatable!
   (table)
   (table [__tostring (const "#<Type>")])))

(define (Type? v)
  (= (get-metatable v) Type))

(define (check-Type v)
  (unless (Type? v)
    (error (format "%s is not a Type" v))))

(define-macro define-type
  [(_ name
      (~or (~optional (~seq #:printed-name printed-name)
                      #:defaults ([printed-name #'name]))
           (~optional (~seq #:predicate-name predicate-name)
                      #:defaults ([predicate-name
                                   (format-id #'name "~a?" #'name)]))
           (~optional (~seq #:index index)
                      #:defaults ([index #'(table)])))
      ...)

   (with-syntax ([name-str (symbol->string (syntax-e #'printed-name))])
     (syntax/loc this-syntax
       (begin
         (define name
           (set-metatable! (table [__index index])
                           Type))

         (define (predicate-name v)
           (and v
                (let ([mt (get-metatable v)])
                  (= mt name)))))))])

(define (ascribe type value)
  (set-metatable! value type))
