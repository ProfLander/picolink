#lang racket

(require (for-syntax racket/base
                     racket/function
                     racket/syntax
                     racket/path
                     
                     syntax/parse

                     picolink/alist
                     picolink/config
                     picolink/scope
                     picolink/artifacts
                     picolink/artifacts/parameters
                     picolink/config/parameters
                     picolink/syntax
                     picolink/backend
                     picolink/backend/parameters
                     picolink/language
                     picolink/module/compiled
                     picolink/module/parameters
                     picolink/program/scope))

(provide (all-defined-out)
         (for-syntax #%input-begin))

(begin-for-syntax
  (define (resolve-module-path search-paths path)
    (for/fold ([acc #f])
              ([search-path (in-list search-paths)]
               #:break acc)

      (cond
        [(path? search-path)

         (let* ([abs-path
                 (build-path search-path path)]

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
                           path)))

         (and (with-handlers ([exn:fail:filesystem:missing-module?
                               (const #f)])
                (module-declared? mod-path #t))

              (with-syntax ([mod-path mod-path])
                #'mod-path))]

        [else
         (error (format "invalid search path: ~a"
                        search-path))]))))

(begin-for-syntax

  (define current-debug
    (make-parameter #t))

  (define (#%input-begin lctx language-path body)

    (define source-path (syntax-source lctx))

    (when (current-debug)
      (printf "~a: #%dependency-begin\n" source-path)
      (displayln ""))

    (define config-path (find-config source-path))

    (unless config-path
      (raise-user-error
       (format "missing ~a file for ~a"
               config-name
               (find-relative-path (current-directory)
                                   (path->string source-path)))))

    (current-config-path config-path)

    (with-syntax ([source-path   source-path]
                  [config-path   (path->string config-path)]
                  [language-path language-path]
                  [(body ...)    body])

      (syntax-local-lift-require

       (syntax-local-introduce
        #`(for-syntax (file config-path)))

       (forward-macro
        lctx
        #'(#%load-language source-path language-path body ...))

       #f))))

(define-syntax #%load-language
  (syntax-parser
    [(_ source-path language-path body ...)

     (define module-path
       (path-replace-extension (find-relative-path
                                (current-root)
                                (syntax-e #'source-path))
                               ""))

     (when (current-debug)
       (printf "~a: #%load-backend+language" module-path)
       (displayln ""))

     (with-syntax ([module-path   (string->symbol
                                   (path->string
                                    module-path))]
                   [#%language    (language-ident this-syntax)])

       (syntax-local-lift-require

        (syntax-local-introduce
         #`(for-syntax (rename language-path #%language #%language)))

        (forward-macro
         this-syntax
         #'(#%require-dependencies module-path body ...))

        #f))]))

(define-syntax #%require-dependencies
  (syntax-parser
    [(_ module-path body ...)

     (when (current-debug)
       (printf "~a: #%require-dependencies\n"
               (syntax->datum #'module-path))
       (displayln ""))

     (define backend (current-backend))
     #;(define backend-mods (backend-modules backend))

     (define language (syntax-local-language this-syntax))
     (define stx      (language-wrap language #'(body ...)))
     (define dep-mods (language-extract language stx))

     (define search-paths (append (list (current-root))
                                  (language-search-paths language)
                                  (backend-search-paths  backend)))

     (define dep-paths
       (for/list ([dependency (in-list dep-mods)])

         (define dep-path (symbol->string (syntax-e dependency)))

         (define path (resolve-module-path search-paths dep-path))

         (unless path
           (raise-syntax-error #f
                               "failed to resolve module"
                               dependency))

         path))

     (with-syntax ([(dep-mod ...) dep-mods]
                   [(dep:#%module ...) (map (λ (path)
                                              (format-id path
                                                         "~a:#%module"
                                                         path))
                                            dep-mods)]
                   [(dep-path ...) dep-paths]
                   [stx stx])

       (syntax-local-lift-require

        (syntax-local-introduce
         #`(for-meta 0 (rename dep-path dep:#%module #%module) ...))

        (forward-macro
         this-syntax
         #'(#%build-module module-path ([dep-mod dep:#%module] ...)
                           stx))

        #f))]))

(define-syntax #%build-module
  (syntax-parser
    [(_ module-path ([dependency dependency-module] ...) stx)

     (when (current-debug)
       (printf "~a: #%build-module"
               (syntax->datum #'module-path))
       (println (syntax-e #'(dependency ...)))
       (displayln ""))

     (define program (make-scope 'program))

     (with-scope program

       (define (propagate-transitive mod)
         (for ([(ident mod) (in-scope (picolink-module-program-scope mod))])

           (unless (lookup ident)
             (bind! ident mod))

           (propagate-transitive mod)))

       (for ([ident (in-list (attribute dependency))]
             [mod   (in-list (attribute dependency-module))])

         (let ([mod (syntax-local-value mod)])

           (unless (lookup ident)
             (bind! ident mod))

           (propagate-transitive mod))))

     (define backend  (current-backend))
     (define language (syntax-local-language this-syntax))

     (parameterize ([current-module-ident      #'module-path]
                    [current-program-scope     program]
                    [current-compile-artifacts (make-alist)])

       (with-module-scopes

         (let* ([stx #'stx]
                [stx (language-compile language backend stx)])

           (backend-compile backend stx)

           (println (current-compile-artifacts))

           (with-syntax ([mod (picolink-module #'module-path
                                               (current-program-scope)
                                               (current-require-scope)
                                               (current-provide-scope)
                                               (current-lexical-scope)
                                               (current-compile-artifacts))])

             (syntax-local-lift-provide
              #'#%module)

             #'(begin
                 (define-syntax #%module mod)

                 (module+ main
                   (println "module main")))))))]))
