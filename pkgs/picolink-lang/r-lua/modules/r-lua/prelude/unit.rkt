#lang picolink/r-lua/base

(require (only r-lua/prelude/definition  define)
         (only r-lua/prelude/type        define-type ascribe)
         (only r-lua/prelude/table       table))

(provide (all-defined))

(define-macro define-unit
  [(_ name:id)
   (with-syntax ([name:type (format-id #'name "~a:type" #'name)]
                 [name? (format-id #'name "~a?" #'name)])

     (syntax/loc this-syntax
       (begin
         (define-type name:type
           #:printed-name name
           #:predicate-name name?)

         (define name (ascribe name:type (table))))))])
