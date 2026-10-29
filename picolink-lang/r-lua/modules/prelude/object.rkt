#lang picolink/r-lua/base

(provide (all-defined))

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

     #'(begin
         (define-struct name [field ...]
           #:constructor-name %name)

         (impl ToString name:type
               (let ([to-string
                      (λ (self)
                        (string-append "#<actor"
                                       (string-append
                                        " " (tostring
                                             (name-ref self field-index)))
                                       ...
                                       ">"))])
                 (s-lua (#%table ["to-string" to-string]))))

         (define-struct name-handle [index]
           #:constructor-name %name-handle)

         (define (name-handle-index self)
           (name-handle-ref self 0))

         (impl ToString name-handle:type
               (let ([to-string (λ (self)
                                  (format "#<actor-handle %s>"
                                          (name-handle-index self)))])
                 (s-lua (#%table ["to-string" to-string]))))

         (define (%check-name self)
           (unless (name? self)
             (error (format "%s is not a %s" self name-str))))

         (define (%check-name-handle self)
           (unless (name-handle? self)
             (error (format "%s is not a %s handle" self name-str))))

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
           (let ([field (if (eq? field nil)
                            (%name-field self)
                            field)]
                 ...)
             (name field ...)))

         (define (name-field handle)
           (%check-name-handle handle)

           (let* ([index (name-handle-index handle)]
                  [inst (vector-ref %names-active index)])

             (when (nil? inst)
               (error (format "invalid handle: %s" handle)))

             (%name-field inst)))
         ...

         (define (name-update handle field ...)
           (%check-name-handle handle)

           (let* ([index (name-handle-index handle)]
                  [inst (vector-ref %names-active index)])

             (when (nil? inst)
               (error (format "invalid handle: %s" handle)))

             (%name-update inst field ...)))

         (define (name-release handle)
           (%check-name-handle handle)

           (let* ([index (name-handle-index handle)]
                  [inst (vector-ref %names-active index)]
                  [pair (cons handle inst)])
             (vector-set! %names-active index (void))
             (%set-name-field! inst (void))
             ...
             (set! %names-inactive (cons pair %names-inactive))))))])
