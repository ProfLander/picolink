#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/function

         picolink/language
         picolink/binding
         picolink/module/parameters)

(provide (all-from-out racket/base
                       picolink/binding
                       picolink/module/parameters)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ (~or (~optional (~seq #:search-paths search-paths)
                        #:defaults ([search-paths #'(const null)]))
             (~optional (~seq #:modules  modules)
                        #:defaults ([modules #'(const null)]))
             (~optional (~seq #:extract  extract))
             (~optional (~seq #:wrap     wrap))
             (~optional (~seq #:inject   inject)
                        #:defaults ([inject #'(const void)]))
             (~optional (~seq #:compiler compiler))
             body)
        ...)

     #:fail-unless (attribute extract)
     "languages must define #:extract"

     #:fail-unless (attribute wrap)
     "languages must define #:wrap"

     #:fail-unless (attribute compiler)
     "languages must define #:compiler"

     #'(#%plain-module-begin
        body ...

        (provide #%language)
        (define #%language
          (make-language #:search-paths search-paths
                         #:modules      modules
                         #:extract      extract
                         #:wrap         wrap
                         #:inject       inject
                         #:compiler     compiler)))]))
