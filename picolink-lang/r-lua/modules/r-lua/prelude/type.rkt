#lang picolink/r-lua/base

(require (only lua
               getmetatable
               setmetatable)

         r-lua/prelude/equality
         r-lua/prelude/function
         r-lua/prelude/interface
         r-lua/prelude/surface
         r-lua/prelude/print)

(provide (all-defined))

(define Type (s-lua (#%table)))

(define (Type? v)
  (eq? (getmetatable v) Type))

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
     #'(begin
         (define name
           (setmetatable (s-lua (#%table))
                         Type))

         (define (predicate-name v)
           (and v
                (eq? (getmetatable v) name)))

         (impl ToString name
               (let ([to-string (λ (self)
                                  name-str)])
                 (s-lua (#%table ["to-string" to-string]))))))])

(define (ascribe type value)
  (setmetatable value type))
