#lang racket/base

(require (for-syntax racket/base
                     racket/set
                     racket/list
                     racket/syntax
                     racket/function
                     racket/sequence
                     racket/path

                     syntax/parse

                     picolink/syntax
                     picolink/config
                     picolink/config/parameters
                     picolink/backend
                     picolink/language
                     picolink/module
                     picolink/program
                     picolink/program/scope
                     picolink/program/syntax
                     picolink/lang/expand-state)

         picolink/lang/source-module)

(provide (all-defined-out)
         (for-syntax #%entry-point-begin))

(begin-for-syntax

  (define current-debug
    (make-parameter #f))

  (define (#%entry-point-begin lctx language-path body)

    (current-is-entry-point #f)

    (define source-path (syntax-source lctx))
    (unless source-path
      (raise-user-error "module has no source path"))

    (define config-path (find-config source-path))
    (unless config-path
      (raise-user-error
       (format "missing ~a file for ~a"
               config-name
               (find-relative-path (current-directory)
                                   (path->string source-path)))))

    (with-syntax* ([config-path (path->string config-path)]

                   [language-path language-path]
                   [#%language (language-ident lctx)]

                   [(body ...) body])

      (syntax-local-lift-require

       (syntax-local-introduce
        #'(for-syntax racket/base
                      (rename language-path #%language #%language)
                      (file config-path)))

       (forward-macro
        lctx
        #'(#%load-backend body ...))

       #f))))

(define-syntax #%load-backend

  (syntax-parser
    [(_ body ...)

     (when (current-debug)
       (displayln "#%load-backend")
       (println this-syntax))

     (define backend-path
       (current-backend))

     (with-syntax* ([backend-path backend-path]
                    [#%backend (backend-ident this-syntax)])

       (syntax-local-lift-require

        (syntax-local-introduce
         #'(for-syntax (rename backend-path #%backend #%backend)))

        (forward-macro
         this-syntax
         #'(#%define-entry-point body ...))

        #f))]))

(define-syntax #%define-entry-point
  (syntax-parser
    [(_ body ...)

     (when (current-debug)
       (displayln "#%define-entry-point")
       (println this-syntax))

     (define mod (make-source-module-syntax this-syntax #'(body ...)))
     (define entry-point (datum->syntax this-syntax 'entry-point))
     (define entry-point-mod (module-ident this-syntax 'entry-point))

     (with-syntax* ([mod mod]
                    [entry-point entry-point]
                    [entry-point:#%module entry-point-mod]

                    [cont
                     (forward-macro
                      this-syntax
                      #'(#%require-dependencies
                         (entry-point)
                         ()))])

       #`(begin
           (define-for-syntax entry-point:#%module mod)
           cont))]))

(define-syntax (#%require-dependencies stx)

  (when (current-debug)
    (displayln "#%require-dependencies")
    (println stx))

  (syntax-parse stx
    [(_ (target rest ...) (required ...))

     (define backend (syntax-local-backend this-syntax))

     (define mod (syntax-local-module #'target))

     (define lang (source-module-language mod))

     (define search-paths (append (list (current-root))
                                  (language-search-paths lang)
                                  (backend-search-paths backend)))

     (define dependencies
       (filter (λ (dep)
                 (and (not (datum=? dep #'target))

                      (not (member dep (attribute rest)
                                   datum=?))

                      (not (member dep (attribute required)
                                   datum=?))))

               (source-module-dependencies mod)))

     (define dependency-modules
       (map module-ident dependencies))

     (define paths
       (for/list ([dependency (in-list dependencies)])

         (define path
           (for/fold ([acc #f])
                     ([search-path (in-list search-paths)]
                      #:break acc)

             (define dep-path (symbol->string (syntax-e dependency)))

             (cond
               [(path? search-path)

                (let* ([abs-path
                        (build-path search-path dep-path)]

                       [abs-path
                        (path-replace-extension abs-path ".rkt")])

                  (and (file-exists? abs-path)
                       (with-syntax ([path (path->string abs-path)])
                         #'(file path))))]

               [(symbol? search-path)
                (define mod-path
                  (string->symbol
                   (string-append (symbol->string search-path)
                                  "/"
                                  dep-path)))

                (and (with-handlers ([exn:fail? (const #f)])
                       (module-declared? mod-path #t))
                     (with-syntax ([mod-path mod-path])
                       #'mod-path))]

               [else
                (error (format "invalid search path: ~a"
                               search-path))])))

         (unless path
           (raise-syntax-error #f
                               "failed to resolve module"
                               dependency))

         path))

     (with-syntax* ([(dependency ...) dependencies]
                    [(dep-mod ...)    dependency-modules]
                    [(path ...)       paths])

       (syntax-local-lift-require
        (syntax-local-introduce
         #'(for-meta 0 (rename path dep-mod #%module) ...))

        (forward-macro
         this-syntax
         #'(#%require-dependencies
            (rest ... dependency ...)
            (target required ...)))

        #f))]

    [(_ () (required ...))
     (forward-macro
      this-syntax
      #'(#%build-program required ...))]))

(define-syntax #%build-program
  (syntax-parser
    [(_ mod ...)

     (when (current-debug)
       (displayln "#%build-program")
       (println this-syntax))

     (define backend
       (syntax-local-backend this-syntax))

     (define program (parse-program (attribute mod)))

     (parameterize ([current-program-scope program])

       (define compiled
         (if (current-parallel)
             (parallel-compile-program program backend)
             (compile-program program backend)))

       (define transformed
         (backend-transform backend this-syntax compiled))

       (define linked
         (backend-link backend this-syntax transformed))

       (define runner
         (backend-run backend this-syntax linked))

       (with-syntax ([#%program (program-ident this-syntax)]
                     [program program]
                     [runner runner])
         #'(begin
             (define-syntax #%program program)
             runner)))]))
