#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/function

         picolink/backend
         picolink/backend/parameters)

(provide (all-from-out racket/base)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ (~or (~optional (~seq #:target       target))
             (~optional (~seq #:search-paths search-paths)
                        #:defaults ([search-paths #'(const null)]))
             (~optional (~seq #:modules modules)
                        #:defaults ([modules #'(const null)]))
             (~optional (~seq #:compile      compile))
             (~optional (~seq #:run          run))
             (~optional (~seq #:interact     interact))
             body)
        ...)

     #:fail-unless (attribute target)
     "backends must define #:target"

     #:fail-unless (attribute compile)
     "backends must define #:compile"

     #:fail-unless (attribute run)
     "backends must define #:run"

     #:fail-unless (attribute interact)
     "backends must define #:interact"

     (syntax/loc this-syntax
       (#%plain-module-begin

        body ...

        (unless (current-backend)
          (current-backend
           (make-backend #:target       target
                         #:search-paths search-paths
                         #:modules      modules
                         #:compile      compile
                         #:run          run
                         #:interact     interact)))))]))
