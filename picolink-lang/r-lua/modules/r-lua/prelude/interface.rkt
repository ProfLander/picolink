#lang picolink/r-lua/base

(require (only lua         get-metatable set-metatable!)
         (only lua/string  format)

         (only lua/ext/math       =)
         (only lua/ext/table      table-ref table-set!)
         (only lua/ext/object     field send)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/condition   unless cond)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/error       error)
         (only r-lua/prelude/function    const)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       and or)
         (only r-lua/prelude/table       table))

(provide (all-defined))

(define Interface
  (set-metatable!
   (table)
   (table [__tostring (const "#<Interface>")])))

(define (Interface? v)
  (and v
       (let ([mt (get-metatable v)])
         (= mt Interface))))

(define (dispatch interface method)

  (λ (self . [: args #%values])

    (let ([mt (or (get-metatable self)
                  (error
                   (format "%s has unrecognized type" self)))])

      (unless mt
        (error (format "%s has no interface storage" self)))

      (let ([iface-name (table-ref interface "%name")])
        (unless (table-ref mt iface-name)
          (error (format "%s does not implement %s"
                         self iface-name))))

      (cond
        [(field self method)
         (send self method args)]

        [else
         ((table-ref (table-ref interface "%member-defaults")
                     method)
          self args)]))))

(define-macro define-interface
  [(_ (~and name
            (~parse name-str (symbol->string
                              (syntax-e #'name))))

      [(~and function
             (~parse function-str (symbol->string
                                   (syntax-e #'function))))
       (~and metamethod
             (~parse metamethod-str (symbol->string
                                     (syntax-e #'metamethod))))
       (~optional default-impl
                  #:defaults
                  ([default-impl
                     #'(λ (self . [: args #%values])
                         (error (format "%s missing required method %s"
                                        self function-str)))]))]
      ...)

   (syntax/loc this-syntax
     (begin
       (define name
         (set-metatable! (table [%name name-str]

                                [%member-names
                                 (table [function metamethod-str] ...)]

                                [%member-defaults
                                 (table [metamethod default-impl] ...)])

                         Interface))

       (define function (dispatch name metamethod-str))
       ...))])

(define-macro impl
  [(_ [interface target]
      ((~datum define) (~or (~and (~seq (method-name arg ...)
                                        body ...)
                                  (~parse method-expr
                                          #'(λ (arg ...) body ...)))
                            (~seq method-name method-expr)))
      ...)

   (with-syntax ([(method-name-str ...)
                  (for/list ([ident (in-list (attribute method-name))])
                    (symbol->string (syntax-e ident)))])
     #'(begin
        (table-set! target (table-ref interface "%name") #t)

        (let ([inner-name (table-ref (table-ref interface "%member-names")
                                     method-name-str)])

          (table-set! (table-ref target "__index")
                      inner-name method-expr))
        ...))])
