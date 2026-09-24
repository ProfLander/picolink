#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         picolink/language
         picolink/intrinsic)

(provide (all-from-out racket/base)
         (all-from-out picolink/intrinsic)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ (~optional (~seq #:module-path (~or path:id
                                            (~and #f(~parse path #'#f))))
                   #:defaults ([path #''same]))
        bind:id ...)
     (make-language-module
      this-syntax
      (with-syntax ([(bind ...) (map syntax-local-identifier-as-binding
                                     (syntax-e #'(bind ...)))])
        #'(make-intrinsic
           #'path
           #'(bind ...))))]))
