#lang picolink/r-lua/base

(provide (all-defined))

(define-macro define-struct
  [(_ name [field:id ...]
      (~or (~optional (~seq #:constructor-name constructor-name)
                      #:defaults ([constructor-name #'name]))
           
           (~optional (~seq #:type-name type-name)
                      #:defaults ([type-name
                                   (format-id #'name "~a:type" #'name)]))

           (~optional (~seq #:predicate-name predicate-name)
                      #:defaults ([predicate-name
                                   (format-id #'name "~a?" #'name)])))
      ...)

   (with-syntax ([name-ref
                  (format-id #'name "~a-ref" #'name)]

                 [(field-str ...)
                  (for/list ([field (in-list (attribute field))])
                    (symbol->string (syntax-e field)))]

                 [(field ...) (attribute field)])

     #'(begin
         (define-type type-name
           #:printed-name name
           #:predicate-name predicate-name)

         (define (constructor-name field ...)
           (ascribe type-name
                    (s-lua (#%table field ...))))

         (define (name-ref self idx)
           (member self (+ idx 1)))

         #;(define (name-set! self idx val)
             (s-lua (#%assign [(#%member self (#%add idx 1))]
                              [val]))
             (void))))])
