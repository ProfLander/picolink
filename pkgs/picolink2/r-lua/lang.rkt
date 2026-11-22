#lang racket/base

(require (for-syntax racket/base
                     racket/path
                     racket/syntax

                     syntax/parse

                     picolink2/lua
                     picolink2/module-path)

         (only-in racket/base
                  [#%module-begin #%base-module-begin]
                  [require base-require]
                  [provide base-provide])

         picolink2/compiler/artifact
         picolink2/import
         picolink2/r-lua)

(provide #%module-begin
         #%top-interaction
         #%top
         require
         provide
         all-defined-out
         for-space
         print

         (all-from-out picolink2/r-lua))

(define-for-syntax (imports-ident lctx module-path)
  (format-id lctx "~a:#%imports" module-path))

(define-for-syntax (sources-ident lctx module-path)
  (format-id lctx "~a:#%sources" module-path))

(define-for-syntax (module-body-ident lctx module-path)
  (format-id lctx "~a:#%module-body" module-path))

(define-for-syntax (thread-ident lctx)
  (datum->syntax lctx '#%thread))

(define-for-syntax (stdout-ident lctx)
  (datum->syntax lctx '#%stdout))

(define-for-syntax (stdin-ident lctx)
  (datum->syntax lctx '#%stdin))

(define-for-syntax (stderr-ident lctx)
  (datum->syntax lctx '#%stderr))

(define-for-syntax (parse-module-form idents definitions forms form)
  (syntax-parse form
    [((~datum racket) body ...)

     (values idents
             definitions

             (cons (syntax/loc this-syntax
                     (begin body ...))
                   forms))]

    [((~or (~literal require)
           (~literal provide))
      _ ...)

     (values idents
             definitions
             (cons this-syntax forms))]

    [((~literal provide) _ ...)

     (values idents
             definitions
             (cons this-syntax forms))]

    [((~datum define) ident form)

     (with-syntax ([ident (in-r-lua #'ident)])

       (values (cons #'ident idents)

               (cons #'ident definitions)

               (cons (syntax/loc this-syntax
                       (define ident
                         (r-lua (define ident form))))
                     forms)))]

    [ident:id

     (with-syntax ([temp (in-r-lua (generate-temporary))])

       (values (cons #'temp idents)

               definitions

               (cons (syntax/loc this-syntax
                       (begin
                         (void (r-lua ident))
                         (define temp
                           (r-lua (print ident)))))
                     forms)))]

    [form

     (with-syntax ([ident (in-r-lua
                           (generate-temporary))])

       (values (cons #'ident idents)

               definitions

               (cons (syntax/loc this-syntax
                       (define ident (r-lua (print form))))
                     forms)))]))

(define-for-syntax (parse-module-body forms)
  
  (let-values ([(idents definitions forms)

                (for/fold ([idents      null]
                           [definitions null]
                           [forms       null])

                          ([form (in-list forms)])

                  (parse-module-form idents definitions forms form))])

    (values (reverse idents)
            (reverse definitions)
            (reverse forms))))

(define-syntax #%module-begin
  (syntax-parser
    [(_ body ...)

     (define source-path (syntax-source this-syntax))
     (define module-path (syntax-module-path this-syntax))

     (printf "building ~a\n"
             (find-relative-path (current-directory) source-path))

     (define-values (idents definitions forms)
       (parse-module-body (attribute body)))

     (with-syntax ([module-path module-path]

                   [(ident ...)      (map in-r-lua idents)]
                   [(definition ...) (map in-r-lua definitions)]
                   [(form ...)       forms]

                   [#%imports     (imports-ident     this-syntax
                                                     module-path)]

                   [#%sources     (sources-ident     this-syntax
                                                     module-path)]

                   [#%module-body (module-body-ident this-syntax
                                                     module-path)]

                   [main          (datum->syntax     this-syntax 'main)]
                   [#%thread      (thread-ident      this-syntax)]
                   [#%stdout      (stdout-ident      this-syntax)]
                   [#%stdin       (stdin-ident       this-syntax)]
                   [#%stderr      (stderr-ident      this-syntax)])

       (syntax/loc this-syntax
         (#%base-module-begin
          form ...

          (define #%imports     (make-hash))
          (define #%sources     (box null))
          (define #%definitions (list 'definition ...))
          (define #%module-body (list ident ...))

          (base-provide #%imports
                        #%sources
                        #%definitions
                        #%module-body)

          (module* link racket/base
            (require (only-in (submod "..")
                              [#%imports     #%imports]
                              [#%sources     #%sources]
                              [#%definitions #%definitions]
                              [#%module-body #%module-body])

                     syntax/location
                     
                     picolink2/compiler/link
                     picolink2/lua)

            (define #%linked
              (parameterize ([current-linker     lua-link/file]
                             [current-link-space 'r-lua])

                (link-module (quote-module-path "..")
                             'module-path
                             #%imports
                             (unbox #%sources)
                             #%definitions
                             #%module-body)))

            (base-provide #%linked))

          (module* main racket/base

            (require racket/file

                     picolink2/compiler
                     picolink2/r-lua
                     picolink2/lua)

            (define-values (#%thread #%stdout #%stdin #%stderr)

              (parameterize ([current-build-directory "build"]
                             [current-runner          lua-run/file])

                (let* ([linked (link)])
                  
                  (write-compile-artifacts!)
                  (printf "\nrunning ~a...\n\n" 'module-path)
                  (run linked 'module-path))))

            (define-syntax #%top-interaction
              (syntax-parser
                [(_ . input)

                 (let* ([input #'(r-lua (print input))]
                        [input (local-expand input 'expression null)])

                   (quasisyntax/loc this-syntax
                     (if (thread-running? #%thread)
                         (display (format "~a\0" #,input) #%stdin)
                         (error "REPL thread is not running"))))]))))))]))

(define-syntax require
  (syntax-parser
    [(_ spec ...)
     (with-syntax
       ([(spec ...)
         (for/list ([spec (in-list (attribute spec))])
           (syntax-parse spec
             [((~datum for-racket) spec ...)
              (syntax/loc this-syntax
                (combine-in spec ...))]

             [form (syntax/loc this-syntax
                     (for-space r-lua
                                (capture-imports form)))]))])

       (syntax/loc this-syntax
         (base-require spec ...)))]))

(define-syntax provide
  (syntax-parser
    [(_ spec ...)
     (with-syntax
       ([(spec ...)
         (for/list ([spec (in-list (attribute spec ))])
           (syntax-parse spec
             [((~datum for-racket) spec ...)
              (syntax/loc this-syntax
                (combine-out spec ...))]

             [form (syntax/loc this-syntax
                     (for-space r-lua form))]))])

       #'(base-provide spec ...))]))

(define-syntax #%top-interaction
  (syntax-parser
    [(_ . ((~datum racket) form))
     #'form]

    [(_ . form)
     #'(r-lua form)]))

