#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/require-transform

                     syntax/parse

                     picolink2/lua
                     picolink2/compiler/module-path)

         (only-in racket/base
                  [#%module-begin #%base-module-begin]
                  [require base-require]
                  [provide base-provide])

         picolink2/compiler/artifact
         picolink2/gaol

         (only-in picolink2/gaol/ir
                  in-gaol
                  gaol
                  for-gaol)

         picolink2/gaol/passes/s-lua)

(provide #%module-begin
         #%top-interaction
         #%top
         require
         provide
         all-defined-out
         print

         (all-from-out picolink2/gaol)
         (all-from-out picolink2/gaol/passes/s-lua))

(define-for-syntax (parse-module-form idents definitions forms form)
  (syntax-parse form
    #:literals       [require provide]
    #:datum-literals [define]

    [((~datum racket) body ...)

     (values idents
             definitions

             (cons (syntax/loc this-syntax
                     (begin body ...))
                   forms))]

    [((~or require provide) _ ...)

     (values idents
             definitions
             (cons this-syntax forms))]

    [(define ident form)

     (with-syntax ([ident (in-gaol #'ident)])

       (values (cons #'ident idents)

               (cons #'ident definitions)

               (cons (syntax/loc this-syntax
                       (define ident
                         (gaol (define ident form))))
                     forms)))]

    [ident:id

     (with-syntax ([temp (in-gaol (generate-temporary))])

       (values (cons #'temp idents)

               definitions

               (cons (syntax/loc this-syntax
                       (begin
                         (void (gaol ident))
                         (define temp
                           (gaol (print ident)))))
                     forms)))]

    [form

     (with-syntax ([ident (in-gaol
                           (generate-temporary))])

       (values (cons #'ident idents)

               definitions

               (cons (syntax/loc this-syntax
                       (define ident (gaol (print form))))
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

     (define module-path (syntax-module-path this-syntax))

     (printf "compiling ~a...\n" module-path)

     (define-values (idents definitions forms)
       (parse-module-body (attribute body)))

     (define requires
       (for/fold ([acc null])
                 ([body (attribute body)]

                  #:do [(define result
                          (syntax-parse body
                            #:datum-literals [require]
                            [(require req-spec ...)
                             (attribute req-spec)]
                            [_ #f]))]

                  #:when result)

         (append acc result)))

     (define-values (imports sources)
       (let-values ([(imports sources)
                     (expand-import #`(combine-in #,@requires))])

         (values (for/fold ([acc (hash)])
                           ([import (in-list imports)]
                            #:when (equal? (cons 0 'gaol)
                                           (import-mode import)))

                   (let* ([src-mod-path (syntax->datum
                                         (import-src-mod-path import))]

                          [src-mod-path (lua-normalize-module-path
                                         src-mod-path)]

                          [src-sym (import-src-sym import)])

                     (hash-update acc src-mod-path

                                  (λ (imports)
                                    (append imports (list src-sym)))

                                  (λ ()
                                    (list src-sym)))))

                 (for/list ([source (in-list sources)])
                   
                   (let ([mod-path-stx
                          (syntax-e
                           (import-source-mod-path-stx source))])

                     (lua-normalize-module-path mod-path-stx))))))

     (with-syntax ([module-path module-path]

                   [(ident ...)      (map in-gaol idents)]
                   [(definition ...) (map in-gaol definitions)]
                   [(form ...)       forms]

                   [#%imports imports]
                   [#%sources sources]

                   [#%module-body (format-id this-syntax
                                             "~a:#%module-body"
                                             module-path)])

       (syntax/loc this-syntax
         (#%base-module-begin

          form ...

          (define #%definitions (list 'definition ...))
          (define #%module-body (list ident ...))

          (base-provide #%definitions
                        #%module-body)

          (module* link racket/base
            (require (for-syntax
                      (only-in (submod "..")
                               [#%definitions #%definitions]
                               [#%module-body #%module-body])

                      (for-space gaol
                       (only-in (submod "..")
                                [definition definition] ...))

                      racket/base
                      syntax/parse
                      syntax/location

                      picolink2/compiler/link
                      picolink2/lua))

            (define-syntax link
              (syntax-parser
                [(_)

                 (parameterize ([current-linker     lua-link/file]
                                [current-link-space 'gaol])

                   (let* ([imports     #%imports]
                          [sources     '#%sources]
                          
                          [exports     (assoc (cons 0 'gaol)
                                              (syntax-local-module-exports
                                               (quote-module-path "..")))]

                          [exports     (if exports
                                           (cdr exports)
                                           null)]

                          [definitions #%definitions]
                          [module-body #%module-body])

                     (with-syntax ([((path . linked) (... ...))
                                    (link-module 'module-path
                                                 imports
                                                 sources
                                                 exports
                                                 definitions
                                                 module-body)])

                       #'(list (cons path linked) (... ...)))))]))

            (define #%linked (link))

            (base-provide #%linked))

          (module* main racket/base

            (require racket/file

                     picolink2/compiler
                     picolink2/gaol/passes/s-lua
                     picolink2/lua)

            (define-values (#%thread #%stdout #%stdin #%stderr)

              (parameterize ([current-build-directory "build"]
                             [current-runner          lua-run/file])

                (let* ([linked (link)])
                  (write-compile-artifacts! linked)
                  (printf "\nrunning ~a...\n\n" 'module-path)
                  (run linked 'module-path))))

            (define-syntax #%top-interaction
              (syntax-parser
                [(_ . input)

                 (let* ([input #'(gaol (print input))]
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
                     (for-gaol form))]))])

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
                     (for-gaol form))]))])

       #'(base-provide spec ...))]))

(define-syntax #%top-interaction
  (syntax-parser
    [(_ . ((~datum racket) form))
     #'form]

    [(_ . form)
     #'(gaol form)]))

