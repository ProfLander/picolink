#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/set

         picolink/language
         picolink/binding
         picolink/s-lua)

(provide (all-from-out racket/base)
         (all-from-out picolink/s-lua)
         #%module-begin)

(define-syntax #%module-begin
  (syntax-parser
    [(_ (~optional (~seq #:provide [prov:id ...]))
        body:expr ...)
     (make-language-module
      this-syntax
      #'(make-s-lua (list #'body ...)
                    #:provides (set (binding #'prov) ...)))]))
