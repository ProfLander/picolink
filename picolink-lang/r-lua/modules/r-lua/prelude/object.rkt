#lang picolink/r-lua/base

(require (only lua/ext/type  nil?)

         (only r-lua/prelude/binding              let*)
         (only r-lua/prelude/condition            when unless)
         (only r-lua/prelude/definition           define)
         (only r-lua/prelude/error                error)
         (only r-lua/prelude/format               format)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/monoid     <>)
         (only r-lua/prelude/interface/update     Update! update!)
         (only r-lua/prelude/interface/drop       Drop drop)
         (only r-lua/prelude/pool                 define-pool)
         (only r-lua/prelude/sequencing           begin)
         (only r-lua/prelude/interface/to-string  ToString to-string)
         (only r-lua/prelude/struct               define-struct))

(provide (all-defined))

(define-macro define-object
  [(_ name:id [field:id ...])

   (define (format-ids name . patterns)
     (for/list ([pat (in-list patterns)])
       (format-id name pat name)))

   (with-syntax ([(%name
                   name?
                   name:type
                   name-ref
                   name-set!
                   %check-name
                   %get-name
                   %get-name-handle)

                  (format-ids #'name
                              "%~a"
                              "~a?"
                              "~a:type"
                              "~a-ref"
                              "~a-set!"
                              "%check-~a"
                              "%get-~a"
                              "%check-~a-handle")]

                 [name-str    (symbol->string (syntax-e #'name))]

                 [(field-index ...)
                  (for/list ([field (in-list (attribute field))]
                             [index (in-naturals)])
                    index)]

                 [(%name-field ...)
                  (for/list ([field (in-list (attribute field))])
                    (format-id #'name "%~a-~a" #'name field))]

                 [(name-field ...)
                  (for/list ([field (in-list (attribute field))])
                    (format-id #'name "~a-~a" #'name field))])

     (define stx (syntax/loc this-syntax
                   (begin
                     (define-struct name [field ...]
                       #:constructor-name %name)

                     (define (%check-name self)
                       (unless (name? self)
                         (error (format "~a is not a ~a" self name-str))))

                     (define (%name-field self)
                       (%check-name self)
                       (name-ref self field-index))
                     ...

                     (impl [Equal name:type]
                           (define (equal? a b)
                             (s-lua (#%eq a b))))

                     (impl [ToString name:type]
                           (define (to-string self)
                             (<> "#<"
                                 name-str
                                 (<>
                                  " " (to-string
                                       (name-ref self
                                                 field-index)))
                                 ...
                                 ">")))
                     
                     (impl [Update! name:type]
                           (define (update! self field ...)
                             (%check-name self)
                             (name-set! self field-index field)
                             ...
                             self))

                     (impl [Drop name:type]
                           (define (drop self)
                             (~@ (drop (name-ref self field-index))
                                 (name-set! self field-index (void)))
                             ...))

                     (define-pool name
                       #:constructor [name %name])

                     (define (name-field handle)
                       #;(%check-name-handle handle)

                       (let* ([inst (%get-name handle)])

                         (when (nil? inst)
                           (error (format "invalid handle: ~a" handle)))

                         (%name-field inst)))
                     ...

                     )))

     (println (syntax->datum stx))

     stx)])
