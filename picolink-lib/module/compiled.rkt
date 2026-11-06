#lang racket/base

(require picolink/ee-lib)

(provide (all-defined-out))

(struct compiled-module [ident
                         language
                         require-scope
                         provide-scope
                         module-scope
                         stx]
  #:transparent)

(define (make-compiled-module ident
                              #:language language
                              #:require-scope require-scope
                              #:provide-scope provide-scope
                              #:module-scope module-scope
                              #:stx stx)
  (compiled-module ident
                   language
                   require-scope
                   provide-scope
                   module-scope
                   stx))

(define (compiled-module-update compiled
                                #:ident
                                [ident
                                 (compiled-module-ident compiled)]
                                #:language
                                [language
                                 (compiled-module-language compiled)]
                                #:require-scope
                                [require-scope
                                 (compiled-module-require-scope compiled)]
                                #:provide-scope
                                [provide-scope
                                 (compiled-module-provide-scope compiled)]
                                #:module-scope
                                [module-scope
                                 (compiled-module-module-scope compiled)]
                                #:stx
                                [stx
                                 (compiled-module-stx compiled)])
  (compiled-module ident
                   language
                   require-scope
                   provide-scope
                   module-scope
                   stx))
