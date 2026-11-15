#lang picolink/r-lua/base

(require (only lua         get-metatable set-metatable!)
         (only lua/string  format)

         (only lua/ext/ops        =)
         (only lua/ext/table      table-ref table-set!)
         (only lua/ext/object     field send)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/condition   unless)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/error       error)
         (only r-lua/prelude/function    const)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       and or)
         (only r-lua/prelude/table       table)
         (only r-lua/prelude/type        check-Type))

(provide (all-defined))

(define Interface
  (set-metatable!
   (table)
   (table [__tostring (const "#<Interface>")])))

(define (Interface? v)
  (and v
       (let ([mt (get-metatable v)])
         (= mt Interface))))

(define (check-Interface v)
  (unless (Interface? v)
    (error (format "%s is not an Interface" v))))

(define (dispatch interface method)

  (check-Interface interface)

  (λ (self . [: args #%values])

    (let ([iface-name (table-ref interface "%name")])
      (unless (field self iface-name)
        (error (format "%s does not implement %s"
                       self iface-name))))

    (let ([method (or (field self method)
                      (table-ref (table-ref interface "%member-defaults")
                                 method))])
      (method self args))))

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

         (check-Interface interface)
         (check-Type      target)

         (let ([index (table-ref target "__index")]
               [name (table-ref interface "%name")]
               [member-names (table-ref interface "%member-names")])

           (table-set! index name #t)

           (let ([inner-name (table-ref member-names method-name-str)])
             (table-set! index inner-name method-expr))
           ...)))])
