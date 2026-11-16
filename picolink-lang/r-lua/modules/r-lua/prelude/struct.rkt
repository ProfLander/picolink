#lang picolink/r-lua/base

(require (only lua print)
         (only lua/string format)

         (only lua/ext/table  table-ref table-set!)

         (only r-lua/runtime/pool
               pool-id
               make-pool
               pool-allocate!
               pool-field
               pool-chunk->string)
         (only r-lua/runtime/pool/handle
               handle?
               handle-pool
               handle-chunk)
         (only r-lua/runtime/pool/type
               Chunk
               copy-handle!
               drop-handle!)

         (only r-lua/prelude/arith                +)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/copy       Copy)
         (only r-lua/prelude/interface/drop       Drop)
         (only r-lua/prelude/interface/equal      Equal equal?)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/logic                and)
         (only r-lua/prelude/sequencing           begin)
         (only r-lua/prelude/table                table)
         (only r-lua/prelude/type                 define-type))

(provide (all-defined))

(define-macro define-struct
  [(_ (~and name:id
            (~parse name-str
                    (symbol->string (syntax-e #'name))))

      [(~and field:id
             (~parse name-field
                     (format-id #'field "~a-~a" #'name #'field))
             (~parse field-str
                     (symbol->string (syntax-e #'field))))
       ...]

      (~or (~optional (~seq #:type name-type))

           (~optional (~seq #:pool-size pool-size)
                      #:defaults ([pool-size #'64]))

           (~optional (~seq #:constructor-name constructor-name)
                      #:defaults ([constructor-name #'name]))

           (~optional (~seq #:predicate-name name?)
                      #:defaults ([name?
                                   (format-id #'name "~a?" #'name)])))
      ...)

   (with-syntax* ([name-pool
                   (format-id #'name "~a-pool" #'name)]

                  [name-type* (if (attribute name-type)
                                 #'name-type
                                 (format-id #'name "~a:type" #'name))]

                  [(type-def ...)
                   (if (attribute name-type)
                       #'()
                       #'((begin
                            (define-type name-type*)

                            (impl [Copy name-type*]
                                  (define copy copy-handle!))

                            (impl [Drop name-type*]
                                  (define drop drop-handle!))

                            (impl [Equal name-type*])

                            (impl [ToString name-type*]
                                  (define to-string
                                    pool-chunk->string)))))])

     (syntax/loc this-syntax
       (begin

         type-def ...

         (define name-pool
           (make-pool name-str name-type* 1 pool-size field-str ...))

         (define (constructor-name field ...)
           (pool-allocate! name-pool field ...))

         (define (name? self)
           (and (handle? self)
                (equal? (handle-pool self)
                        (pool-id name-pool))))

         (define (name-field self)
           (let ([chunk-index (handle-chunk self)])
             (pool-field name-pool field-str chunk-index)))
         ...)))])
