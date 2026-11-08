#lang picolink/r-lua/base

(require (only lua/ext/table  table-ref table-set!)

         (only r-lua/prelude/arith       +)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/table       table)
         (only r-lua/prelude/type        define-type ascribe))

(provide (all-defined))

(define-macro define-struct
  [(_ name:id [field:id ...]
      (~or (~optional (~seq #:constructor-name constructor-name)
                      #:defaults ([constructor-name #'name]))
           
           (~optional (~seq #:type-name type-name)
                      #:defaults ([type-name
                                   (format-id #'name "~a:type" #'name)]))

           (~optional (~seq #:printed-name printed-name)
                      #:defaults ([printed-name
                                   (format-id #'name "#<~a>" #'name)]))

           (~optional (~seq #:predicate-name predicate-name)
                      #:defaults ([predicate-name
                                   (format-id #'name "~a?" #'name)])))
      ...)

   (with-syntax ([name-ref
                  (format-id #'name "~a-ref" #'name)]

                 [name-set!
                  (format-id #'name "~a-set!" #'name)])

     (syntax/loc this-syntax
       (begin
         (define-type type-name
           #:printed-name printed-name
           #:predicate-name predicate-name)

         (define (constructor-name field ...)
           (ascribe type-name
                    (table field ...)))

         (define (name-ref self idx)
           (table-ref self (+ idx 1)))

         (define (name-set! self idx val)
           (table-set! self (+ idx 1) val)))))])
