#lang racket

(require (for-syntax racket/base
                     syntax/parse
                     picolink/language)

         picolink/lang/source-module)

(provide (all-defined-out)
         (for-syntax #%dependency-begin))

(begin-for-syntax

  (define current-debug
    (make-parameter #t))

  (define (#%dependency-begin lctx language body)

    (when (current-debug)
      (displayln "#%dependency-begin")
      (displayln ""))

    (with-syntax ([#%language (language-ident lctx)]
                  [(body ...) body])

      (syntax-local-lift-require

       (syntax-local-introduce
        #`(for-syntax (rename #,language #%language #%language)))

       #`(#%define-dependency #,language #%language body ...)

       #f))))

(define-syntax #%define-dependency
  (syntax-parser
    [(_ language #%language body ...)

     (with-syntax ([mod (make-source-module-syntax #'#%language
                                                   #'(body ...))])

       (when (current-debug)
         (displayln "#%define-dependency")
         (println (syntax->datum this-syntax))
         (displayln ""))

       (syntax-local-lift-provide
        #'(for-syntax #%module))

       (syntax-local-lift-module-end-declaration
        #'(begin-for-syntax
            (define #%module mod)))

       (syntax-local-lift-require
        #'(for-syntax (rename language #%language #%language))
        #'(begin)
        #f))]))
