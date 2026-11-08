#lang picolink/r-lua/base

(require (only lua/ext/metatable
               get-metatable
               set-metatable!
               metatable-send)

         (only lua/ext/object
               send)

         r-lua/prelude/binding
         r-lua/prelude/definition
         r-lua/prelude/function
         r-lua/prelude/lambda
         r-lua/prelude/logic
         r-lua/prelude/s-lua
         r-lua/prelude/void)

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
  [(_ name [(~and function
                  (~parse function-str (symbol->string (syntax-e #'function))))
            (~and metamethod
                  (~parse metamethod-str (symbol->string (syntax-e #'metamethod))))
            (~optional dispatcher
                       #:defaults ([dispatcher
                                    #'(λ (self)
                                        (metatable-send self metamethod-str))]))] ...)

   (syntax/loc this-syntax
     (begin
       (define name
         (set-metatable! (s-lua (#%table [function-str metamethod-str] ...))
                         Interface))

       (define function dispatcher)
       ...))])

#;(define-macro impl
    [(_ interface target
        ((~datum define) (method-name arg ...)
                         body ...)
        ...)
     #'(begin
         (let ([inner-name (s-lua (#%member interface 'method-name))])
           (s-lua (#%assign [(#%member target  'inner-name)]
                            [(#%member methods 'method-name)])))
         ...)])

(define impl
  (s-lua
   (#%function (interface target methods)
               (#%block
                (#%for [k v (#%in (#%call pairs interface))]
                       (#%block
                        (#%assign [(#%member target 'v)]
                                  [(#%member methods 'k)])))
                (#%return %void)))))
