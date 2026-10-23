#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/function

         picolink/backend)

(provide (all-from-out racket/base)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ (~or (~optional (~seq #:target       target))
             (~optional (~seq #:search-paths search-paths)
                        #:defaults ([search-paths #'(const null)]))
             (~optional (~seq #:transform    transform)
                        #:defaults ([transform #'(λ (a b) b)]))
             (~optional (~seq #:link         link))
             (~optional (~seq #:run          run))
             (~optional (~seq #:interact     interact))
             body)
        ...)

     #:fail-unless (attribute target)
     "backends must define #:target"

     #:fail-unless (attribute link)
     "backends must define #:link"

     #:fail-unless (attribute run)
     "backends must define #:run"

     #:fail-unless (attribute interact)
     "backends must define #:interact"

     #'(#%plain-module-begin
        body ...

        (provide #%backend)
        (define #%backend
          (make-backend #:target       target
                        #:search-paths search-paths
                        #:transform    transform
                        #:link         link
                        #:run          run
                        #:interact     interact)))]))
