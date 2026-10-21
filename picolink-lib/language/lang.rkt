#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         picolink/ee-lib
         picolink/parser
         picolink/module/scopes
         picolink/language)

(provide (all-from-out racket/base
                       picolink/ee-lib
                       picolink/parser
                       picolink/module/scopes)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ (~or (~optional (~seq #:extract  extract))
             (~optional (~seq #:wrap     wrap))
             (~optional (~seq #:parse    parse))
             (~optional (~seq #:compiler compiler))
             body)
        ...)

     #:fail-unless (attribute extract)
     "languages must define #:extract"

     #:fail-unless (attribute wrap)
     "languages must define #:wrap"

     #:fail-unless (attribute parse)
     "languages must define #:parse"

     #:fail-unless (attribute compiler)
     "languages must define #:compiler"

     #'(#%plain-module-begin
        body ...

        (provide #%language)
        (define #%language
          (make-language #:extract  extract
                         #:wrap     wrap
                         #:parse    parse
                         #:compiler compiler)))]))
