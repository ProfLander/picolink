#lang racket/base

(require (for-syntax racket/base
                     racket/require-transform
                     racket/syntax

                     syntax/parse

                     picolink2/lua
                     picolink2/module-path)

         racket/function
         racket/set)

(provide (all-defined-out))

(define-syntax capture-imports
  (make-require-transformer
   (syntax-parser

     [(_ stx)

      (define module-path (syntax-module-path #'stx))

      (define-values (imports sources)
        (expand-import #'stx))

      (define imports*

        (for/list ([import (in-list imports)]
                   #:when (equal? (import-mode import) (cons 0 'r-lua)))

          (let ([mod-path (syntax->datum (import-src-mod-path import))]
                [src-sym  (import-src-sym import)])

            (cons (lua-normalize-module-path mod-path)
                  src-sym))))

      (define sources*
        (for/list ([source (in-list sources)])

          (let ([mod-path
                 (syntax->datum (import-source-mod-path-stx source))])

            (lua-normalize-module-path mod-path))))

      (syntax-local-lift-module-end-declaration
       (with-syntax ([#%imports (format-id #'stx "~a:#%imports"
                                           module-path)]

                     [((import-mod . import) ...) imports*]

                     [#%sources (format-id #'stx "~a:#%sources"
                                           module-path)]

                     [(source ...) sources*])
         #'(begin
             (hash-update! #%imports 'import-mod
                           (curryr set-add 'import)
                           (λ () (set 'import)))
             ...
             (set-box! #%sources
                       (append (unbox #%sources)
                               '(source ...))))))

      (values imports sources)])))
