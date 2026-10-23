#lang racket

(require (for-syntax racket/base
                     syntax/parse
                     picolink/language)

         picolink/lang/source-module)

(provide (all-defined-out)
         (for-syntax #%dependency-begin))

(begin-for-syntax

  (define current-debug
    (make-parameter #f))

  (define (#%dependency-begin lctx language body)

    (with-syntax ([#%language (language-ident lctx)]
                  [(body ...) body])

      #`(begin
          (require (for-syntax (only-in #,language #%language)))
          (#%define-dependency #,language #%language body ...)))))

(define-syntax #%define-dependency
  (syntax-parser
    [(_ language #%language body ...)

     (with-syntax ([mod (make-source-module-syntax #'#%language
                                                   #'(body ...))])

       (when (current-debug)
         (displayln "#%define-dependency")
         (println this-syntax))

       #'(begin
           (require (for-syntax (only-in language #%language)))

           (provide (for-syntax #%module))

           (define-for-syntax #%module
             mod)))]))
