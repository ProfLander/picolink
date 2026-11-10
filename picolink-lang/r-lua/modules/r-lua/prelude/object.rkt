#lang picolink/r-lua/base

(require (only lua/ext/nil   nil)
         (only lua/ext/type  nil?)

         (only r-lua/prelude/assignment     set!)
         (only r-lua/prelude/definition     define)
         (only r-lua/prelude/equality       equal?)
         (only r-lua/prelude/error          error)
         (only r-lua/prelude/format         format)
         (only r-lua/prelude/interface      impl)
         (only r-lua/prelude/interface/drop Drop drop)
         (only r-lua/prelude/list           null pair? cons car cdr)
         (only r-lua/prelude/sequencing     begin)
         (only r-lua/prelude/string
               ToString
               to-string
               string-append)
         (only r-lua/prelude/struct         define-struct)
         (only r-lua/prelude/table          table)
         (only r-lua/prelude/vector
               vector
               vector-length
               vector-ref
               vector-set!
               vector-push))

(provide (all-defined))

(define-macro define-pool
  [(_ name:id %make-name:id %set-name-fields!:id)
   (with-syntax ([name-str           (symbol->string (syntax-e #'name))]

                 [%name-handle       (format-id #'name
                                                "%~a-handle"
                                                #'name)]

                 [name-handle        (format-id #'name
                                                "~a-handle"
                                                #'name)]

                 [name-handle-str    (format "~a-handle"
                                             (syntax-e #'name))]

                 [name-handle:type   (format-id #'name
                                                "~a-handle:type"
                                                #'name)]
                 
                 [name-handle-ref    (format-id #'name
                                                "~a-handle-ref"
                                                #'name)]

                 [name-handle-index  (format-id #'name
                                                "~a-handle-index"
                                                #'name)]

                 [name-handle?       (format-id #'name
                                                "~a-handle?"
                                                #'name)]

                 [%check-name-handle (format-id #'name
                                                "%check-~a-handle"
                                                #'name)]

                 [%names-inactive    (format-id #'name
                                                "%~as-inactive"
                                                #'name)]

                 [%names-active      (format-id #'name
                                                "%~as-active"
                                                #'name)]

                 [make-name          (format-id #'name
                                                "make-~a"
                                                #'name)]

                 [set-name-fields!   (format-id #'name
                                                "set-~a-fields!"
                                                #'name)])

     (define stx
       (syntax/loc this-syntax
         (begin
           (define-struct name-handle [index]
             #:constructor-name %name-handle)

           (define (name-handle-index self)
             (name-handle-ref self 0))

           (impl ToString name-handle:type
                 (define (to-string self)
                   (format "#<~a ~a>"
                           name-handle-str
                           (name-handle-index self))))

           (define (%check-name-handle self)
             (unless (name-handle? self)
               (error (format "~a is not a ~a handle" self name-str))))

           (define %names-inactive null)
           (define %names-active (vector))

           (impl Drop name-handle:type
                 (define (drop self)
                   (%check-name-handle self)

                   (let* ([index (name-handle-index self)]
                          [inst  (vector-ref %names-active index)]
                          [pair  (cons self inst)])
                     (vector-set! %names-active index (void))
                     (drop inst)
                     (set! %names-inactive
                           (cons pair %names-inactive))))))))

     (println (syntax->datum stx))

     stx)])

(define-macro define-object
  [(_ name:id [field:id ...])

   (with-syntax ([%name      (format-id #'name "%~a" #'name)]
                 [name?      (format-id #'name "~a?" #'name)]
                 [name-str   (symbol->string (syntax-e #'name))]
                 [name:type
                  (format-id #'name "~a:type" #'name)]

                 [name-ref   (format-id #'name "~a-ref" #'name)]
                 [name-set!  (format-id #'name "~a-set!" #'name)]
                 [%check-name  (format-id #'name "%check-~a" #'name)]

                 [name-handle-str (format "~a-handle" (syntax-e #'name))]
                 [name-handle? (format-id #'name "~a-handle?" #'name)]
                 [%check-name-handle
                  (format-id #'name "%check-~a-handle" #'name)]

                 [(field-index ...)
                  (for/list ([field (in-list (attribute field))]
                             [index (in-naturals)])
                    index)]

                 [(%name-field ...)
                  (for/list ([field (in-list (attribute field))])
                    (format-id #'name "%~a-~a" #'name field))]

                 [(name-field ...)
                  (for/list ([field (in-list (attribute field))])
                    (format-id #'name "~a-~a" #'name field))]

                 [(%set-name-field! ...)
                  (for/list ([field (in-list (attribute field))])
                    (format-id #'name "%set-~a-~a!" #'name field))]

                 [%name-update (format-id #'name "%~a-update" #'name)]
                 [name-update (format-id #'name "~a-update" #'name)]
                 [name-release (format-id #'name "~a-release" #'name)]

                 [%names-inactive
                  (format-id #'name "%~as-inactive" #'name)]
                 [%names-active
                  (format-id #'name "%~as-active" #'name)]

                 [%name-handle (format-id #'name "%~a-handle" #'name)]
                 [name-handle (format-id #'name "~a-handle" #'name)]
                 [name-handle:type
                  (format-id #'name "~a-handle:type" #'name)]

                 [name-handle-ref
                  (format-id #'name "~a-handle-ref" #'name)]
                 [name-handle-index
                  (format-id #'name "~a-handle-index" #'name)])

     (syntax/loc this-syntax
       (begin
         (define-struct name [field ...]
           #:constructor-name %name)

         (impl ToString name:type
               (define (to-string self)
                 (string-append "#<actor"
                                (string-append
                                 " " (to-string
                                      (name-ref self
                                                field-index)))
                                ...
                                ">")))

         (define-struct name-handle [index]
           #:constructor-name %name-handle)

         (define (name-handle-index self)
           (name-handle-ref self 0))

         (impl ToString name-handle:type
               (define (to-string self)
                 (format "#<~a ~a>"
                         name-handle-str
                         (name-handle-index self))))

         (define (%check-name self)
           (unless (name? self)
             (error (format "~a is not a ~a" self name-str))))

         (define (%check-name-handle self)
           (unless (name-handle? self)
             (error (format "~a is not a ~a handle" self name-str))))

         (define (%name-field self)
           (%check-name self)
           (name-ref self field-index))
         ...

         (define (%set-name-field! self val)
           (%check-name self)
           (name-set! self field-index val))
         ...

         (define %names-inactive null)
         (define %names-active (vector))

         (define (name field ...)
           (let* ([handle
                   (if (pair? %names-inactive)

                       (let* ([pair   (car %names-inactive)]
                              [handle (car pair)]
                              [index  (name-handle-index handle)]
                              [inst   (cdr pair)])

                         (set! %names-inactive (cdr %names-inactive))

                         (%set-name-field! inst field)
                         ...

                         (vector-set! %names-active index inst)
                         handle)

                       (let ([index (vector-length %names-active)]
                             [inst  (%name field ...)])
                         (vector-push %names-active inst)
                         (%name-handle index)))])

             handle))
         
         (define (%name-update self field ...)
           (%check-name self)
           (let ([field (if (equal? field nil)
                            (%name-field self)
                            field)]
                 ...)
             (name field ...)))

         (define (name-field handle)
           (%check-name-handle handle)

           (let* ([index (name-handle-index handle)]
                  [inst (vector-ref %names-active index)])

             (when (nil? inst)
               (error (format "invalid handle: ~a" handle)))

             (%name-field inst)))
         ...

         (define (name-update handle field ...)
           (%check-name-handle handle)

           (let* ([index (name-handle-index handle)]
                  [inst (vector-ref %names-active index)])

             (when (nil? inst)
               (error (format "invalid handle: ~a" handle)))

             (%name-update inst field ...)))

         (impl Drop name-handle:type
               (define (drop self)
                 (%check-name-handle self)

                 (let* ([index (name-handle-index self)]
                        [inst (vector-ref %names-active index)]
                        [pair (cons self inst)])
                   (vector-set! %names-active index (void))
                   (%set-name-field! inst (void))
                   ...
                   (set! %names-inactive
                         (cons pair %names-inactive))))))))])
