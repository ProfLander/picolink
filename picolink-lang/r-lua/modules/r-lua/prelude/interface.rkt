#lang picolink/r-lua/base

(require (only lua get-metatable  set-metatable!)
         (only lua/ext/metatable  metatable-ref)
         (only lua/ext/table      table-ref table-set!)
         (only lua/ext/object     field send)

         (only r-lua/prelude/binding     let)
         (only r-lua/prelude/condition   unless cond)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/error       error)
         (only r-lua/prelude/format      format)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/lambda      λ)
         (only r-lua/prelude/logic       and or)
         (only r-lua/prelude/s-lua       s-lua)
         (only r-lua/prelude/table       table))

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

(define (dispatch interface method)

  (λ (self . [: args #%values])

    (let ([mt (or (get-metatable self)
                  (error
                   (format "~a has unrecognized type" self)))])

      (unless mt
        (error (format "~a has no interface storage" self)))

      (let ([iface-name (table-ref interface "%name")])
        (unless (table-ref mt iface-name)
          (error (format "~a does not implement ~a"
                         self iface-name))))

      (cond
        [(and (table-ref interface "%raw")
              (table-ref mt method))
         ((table-ref mt method) self args)]

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

      (~optional (~and #:raw (~bind [raw #'#t]))
                 #:defaults ([raw #'#f]))

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
                         (error (format "~a missing required method ~a"
                                        self function-str)))]))]
      ...)

   (syntax/loc this-syntax
     (begin
       (define name
         (set-metatable! (table [%name name-str]

                                [%raw raw]

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

          (table-set! (if (table-ref interface "%raw")
                          target
                          (table-ref target "__index"))

                      inner-name method-expr))
        ...))])
