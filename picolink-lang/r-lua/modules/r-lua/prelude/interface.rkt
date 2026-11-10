#lang picolink/r-lua/base

(require (only lua/ext/metatable
               get-metatable
               set-metatable!
               metatable-send)

         (only lua/ext/table   table-ref table-set!)
         (only lua/ext/object  send)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       and)
         (only r-lua/prelude/s-lua       s-lua)
         (only r-lua/prelude/table       table)
         (only r-lua/prelude/void        void))

(provide (all-defined))

(define Interface
  (set-metatable!
   (s-lua (#%table))
   (s-lua (#%table ["__tostring"
                    (#%function ()
                                (#%block
                                 (#%return "#<Interface>")))]))))

(define (Interface? v)
  (and v
       (let ([mt (get-metatable v)])
         (s-lua (#%eq mt Interface)))))

(define-macro define-interface
  [(_ name [function
            (~and metamethod
                  (~parse metamethod-str (symbol->string
                                          (syntax-e #'metamethod))))
            (~optional dispatcher
                       #:defaults
                       ([dispatcher
                         #'(λ (self)
                             (metatable-send self
                                             metamethod-str))]))] ...)

   (syntax/loc this-syntax
     (begin
       (define name
         (set-metatable! (table [function metamethod-str] ...)
                         Interface))

       (define function dispatcher)
       ...))])

(define-macro impl
  [(_ interface target
      ((~datum define) (~or (~and (~seq (method-name arg ...)
                                        body ...)
                                  (~parse method-expr
                                          #'(λ (arg ...) body ...)))
                            (~seq method-name method-expr)))
      ...)

   (with-syntax ([(method-name-str ...)
                  (for/list ([ident (in-list (attribute method-name))])
                    (symbol->string (syntax-e ident)))])
     #'(void
        (let ([inner-name (table-ref interface method-name-str)])
          (table-set! target inner-name method-expr))
        ...))])
