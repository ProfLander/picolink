#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
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

(define-syntax #%require-dependencies
  (syntax-parser
    [(_ (target rest ...) (required ...))

     (when (current-debug)
       (displayln "#%require-dependencies")
       (println this-syntax))

     (define backend (syntax-local-backend this-syntax))

     (define mod (syntax-local-module #'target))

     (define dependencies
       (filter (λ (dep)
                 (and (not (member dep (attribute rest)
                                   (λ (a b)
                                     (equal? (syntax-e a)
                                             (syntax-e b)))))

                      (not (member dep (attribute required)
                                   (λ (a b)
                                     (equal? (syntax-e a)
                                             (syntax-e b)))))
                      (not (member dep (backend-modules backend)
                                   (λ (a b)
                                     (equal? (syntax-e a)
                                             (syntax-e (car b))))))))
               (source-module-dependencies mod)))

     (define dependency-modules
       (map module-ident dependencies))

     (define paths
       (let ([config-root (current-root)])
         (for/list ([dependency (in-list dependencies)])
           (let* ([dep-path (symbol->string (syntax-e dependency))]
                  [abs-path (build-path config-root dep-path)]
                  [abs-path (path-replace-extension abs-path ".rkt")])

             (unless (file-exists? abs-path)
               (raise-syntax-error #f
                                   "failed to resolve module"
                                   dependency))

             (path->string abs-path)))))

     (with-syntax* ([(dependency ...) dependencies]
                    [(dep-mod ...)    dependency-modules]
                    [(path ...)       paths])

       (syntax-local-lift-require
        (syntax-local-introduce
         #'(for-meta 0 (rename (file path) dep-mod #%module) ...))

        (forward-macro
         this-syntax
         #'(#%require-dependencies
            (rest ... dependency ...)
            (target required ...)))

        #f))]

    [(_ () (required ...))
     (forward-macro
      this-syntax
      #'(#%load-backend-modules required ...))]))

(define-syntax #%load-backend-modules
  (syntax-parser
    [(_ mod ...)

     (when (current-debug)
       (displayln "#%load-backend-modules")
       (println this-syntax))

     (define backend (syntax-local-backend this-syntax))
     (define modules (backend-modules backend))

     (with-syntax* ([([name name:#%module path] ...)
                     (for/list ([pair (in-list modules)])
                       (let* ([name (car pair)]
                              [name (syntax-shift-phase-level
                                     (datum->syntax this-syntax name)
                                     1)]
                              [name:#%module (module-ident name)]
                              [path (cdr pair)])
                         (list name name:#%module path)))])

       (syntax-local-lift-require

        (syntax-local-introduce
         #'(for-meta 0 (rename path name:#%module #%module) ...))

        (forward-macro
         this-syntax
         #'(#%build-program name ... mod ...))

        #f))]))

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
