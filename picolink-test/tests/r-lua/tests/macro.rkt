#lang picolink/r-lua

(let* ([x "foo"]
       [y x])
  y)

(when #t
  (print "bar")
  "hello")

(unless #f
  (print "baz")
  "world")

(define-macro define-struct
  [(_ name [field:id ...])

   (with-syntax ([name:struct
                  (format-id #'name "~a:struct" #'name)]

                 [(field-str ...)
                  (for/list ([field (in-list (attribute field))])
                    (symbol->string (syntax-e field)))]

                 [(field ...) (attribute field)])

     #'(begin
         (define name:struct
           (s-lua (#%table field-str ...)))

         (define name
           (λ (field ...)
             (s-lua (#%table field ...))))))])

(define-struct foo [bar baz])

foo:struct

(foo 1234 6789)
