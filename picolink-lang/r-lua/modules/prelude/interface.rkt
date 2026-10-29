#lang picolink/r-lua/base

(require (only lua
               getmetatable
               setmetatable)

         prelude/equality
         prelude/function
         prelude/logic
         prelude/print
         prelude/surface
         prelude/void)

(provide (all-defined))

(define Interface (s-lua (#%table)))

(define (Interface? v)
  (and v (eq? (getmetatable v) Interface)))

(define-macro define-interface
  [(_ name ([from-name to-name] ...))
   (with-syntax ([(from-name ...)
                  (for/list ([ident (in-list (attribute from-name))])
                    (symbol->string (syntax-e ident)))]
                 [(to-name ...)
                  (for/list ([ident (in-list (attribute to-name))])
                    (symbol->string (syntax-e ident)))])
     #'(define name
         (setmetatable (s-lua (#%table [from-name to-name] ...))
                       Interface)))])

(define-interface Add       ([add        __add]))
(define-interface Subtract  ([subtract   __sub]))
(define-interface Multiply  ([multiply   __mul]))
(define-interface Divide    ([divide     __div]))
(define-interface Modulo    ([modulo     __mod]))
(define-interface Power     ([power      __pow]))
(define-interface Negate    ([negate     __unm]))
(define-interface Concat    ([concat     __concat]))
(define-interface Length    ([length     __len]))
(define-interface Equal     ([equal      __eq]))
(define-interface LessThan  ([less-than  __lt]))
(define-interface LessEqual ([less-equal __le]))
(define-interface ToString  ([to-string  __tostring]))
(define-interface Index     ([index      __index]))
(define-interface NewIndex  ([new-index  __index]))
(define-interface Call      ([call       __call]))

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

(impl ToString Interface
      (let ([to-string (const "Interface")])
        (s-lua (#%table ["to-string" to-string]))))
