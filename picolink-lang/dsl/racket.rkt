#lang racket/base

(require syntax/parse
         picolink/syntax
         ee-lib
         picolink/racket
         (for-template racket/base
                       picolink/dsl/ir))

(provide (all-defined-out))

(define (splice/introduce stx)
  (racket-introduce
   (splice-from-scope stx (current-def-ctx))))

(define dsl-compile/racket
  (syntax-parser
    #:literal-sets [dsl-ir-literals]

    [(#%begin (~cata body dsl-compile/racket) ...)
     #'(begin body ...)]

    [(#%define (~cata name splice/introduce)
               (~cata expr dsl-compile/racket))

     #'(define name expr)]

    [(#%define-macro _ ...)
     (error "unsupported")]

    [(#%let ([(~cata ident splice/introduce)
              (~cata expr dsl-compile/racket)])
            (~cata body dsl-compile/racket) ...)
     #'(let ([ident expr]) body ...)]

    [(~cata ident:id splice/introduce)
     #'ident]

    [n:number
     #'(#%datum . n)]))
