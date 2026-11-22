#lang racket/base

(require (for-syntax racket/base
                     racket/file
                     racket/path

                     syntax/parse

                     picolink/syntax
                     picolink/config
                     picolink/config/parameters))

(provide (for-syntax (all-defined-out)))

(begin-for-syntax

  (define current-debug
    (make-parameter #t))

  (define (#%compiler-begin lctx language stx)

    (define source-path (syntax-source lctx))
    (define config-path (find-config source-path))

    (unless config-path
      (raise-user-error
       (format "missing ~a file for ~a"
               config-name
               (find-relative-path (current-directory)
                                   (path->string source-path)))))

    (current-config-path config-path)

    (with-syntax ([config-path (path->string config-path)]
                  [source-path source-path])

      (syntax-local-lift-require

       (syntax-local-introduce
        #`(for-syntax (file config-path)))

       (forward-macro
        lctx
        #'(#%compile source-path))

       #f))))

(define-syntax #%compile
  (syntax-parser
    [(_ source-path)

     (define module-path
       (path-replace-extension (find-relative-path
                                (current-root)
                                (syntax-e #'source-path))
                               ""))

     (when (current-debug)
       (printf "~a: #%compile" module-path)
       (displayln ""))

     (define files
       (find-files (λ (path)
                     (and (not (equal? path (current-config-path)))
                          (path-has-extension? path ".rkt")))

                   (current-root)))

     (displayln "files:")
     (println files)

     (define verbose #t)
     
     (syntax-local-lift-require

      (syntax-local-introduce
       #'setup/parallel-build)

      #`(parallel-compile-files
         (list #,@files)
         #:handler
         (λ (_worker-id handler-type path msg out err)
           (case handler-type
             ['start (when #,verbose (printf " Compiling ~a\n" path))]
             ['done (when #,verbose (printf " Made ~a\n" path))]
             ['output (printf " Output from: ~a\n~a~a" path out err)]
             [else (printf " Error compiling ~a\n~a\n~a~a"
                           path
                           msg
                           out
                           err)])))

      #f)]))
