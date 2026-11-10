#lang picolink/r-lua/base

(require (only r-lua/prelude/binding              let let*)
         (only r-lua/prelude/condition            when unless)
         (only r-lua/prelude/format               format)
         (only r-lua/prelude/interface            impl)
         (only r-lua/prelude/interface/drop       Drop drop)
         (only r-lua/prelude/interface/length     length)
         (only r-lua/prelude/interface/to-string  ToString)
         (only r-lua/prelude/interface/update     Update update)
         (only r-lua/prelude/list                 null pair? car cdr)
         (only r-lua/prelude/sequencing           begin)
         (only r-lua/prelude/struct               define-struct)
         (only r-lua/prelude/vector
               vector
               vector-ref
               vector-set!
               vector-push))

(provide (all-defined))

(define-macro define-pool
  [(_ name:id
      #:constructor (~or (~and %make-name:id
                               (~parse make-name
                                       (format-id #'name
                                                "make-~a"
                                                #'name)))
                         [make-name:id %make-name:id]))

   (define (format-ids name . patterns)
     (for/list ([pat (in-list patterns)])
       (format-id name pat name)))

   (with-syntax* ([(%name-handle
                    name-handle
                    name-handle:type
                    name-handle-ref
                    name-handle-index
                    name-handle?
                    %check-name-handle
                    %names-inactive
                    %names-active
                    %get-name)

                   (format-ids #'name
                               "%~a-handle"
                               "~a-handle"
                               "~a-handle:type"
                               "~a-handle-ref"
                               "~a-handle-index"
                               "~a-handle?"
                               "%check-~a-handle"
                               "%~as-inactive"
                               "%~as-active"
                               "%get-~a")]

                  [name-str
                   (symbol->string (syntax-e #'name))]
                  
                  [name-handle-str
                   (symbol->string (syntax-e #'name-handle))])

     (define stx (syntax/loc this-syntax
                   (begin
                     (define-struct name-handle [index]
                       #:constructor-name %name-handle)

                     (define (name-handle-index self)
                       (name-handle-ref self 0))

                     (impl [Equal name-handle:type]
                           (define (equal? a b)
                             (s-lua (#%eq a b))))

                     (impl [ToString name-handle:type]
                           (define (to-string self)
                             (format "#<~a ~a>"
                                     name-handle-str
                                     (name-handle-index self))))

                     (define (%check-name-handle self)
                       (unless (name-handle? self)
                         (error (format "~a is not a ~a handle"
                                        self name-str))))

                     (define %names-inactive null)
                     (define %names-active (vector))

                     (define (%get-name handle)
                       (let ([index (name-handle-index handle)])
                         (vector-ref %names-active index)))

                     (define (make-name . [: fields #%values])
                       (let* ([handle
                               (if (pair? %names-inactive)

                                   (let* ([pair   (car %names-inactive)]
                                          [handle (car pair)]

                                          [index
                                           (name-handle-index handle)]

                                          [inst   (cdr pair)])

                                     (set! %names-inactive
                                           (cdr %names-inactive))

                                     (update! inst fields)

                                     (vector-set! %names-active
                                                  index
                                                  inst)
                                     handle)

                                   (let ([index
                                          (length %names-active)]

                                         [inst  (%make-name fields)])

                                     (vector-push %names-active inst)
                                     (%name-handle index)))])

                         handle))

                     (impl [Update name-handle:type]
                           (define (update self . [: fields #%values])
                             (let* ([inst  (%get-name self)])
                               (make-name fields))))

                     (impl [Drop name-handle:type]
                           (define (drop self)
                             (let* ([index (name-handle-index self)]
                                    [inst  (%get-name self)]
                                    [pair  (cons self inst)])
                               (vector-set! %names-active index (void))
                               (drop inst)
                               (set! %names-inactive
                                     (cons pair %names-inactive))))))))

     (println (syntax->datum stx))

     stx)])
