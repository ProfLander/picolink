#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined)
         (all-from r-lua/prelude/surface))

(define-macro define-intrinsics
  [(_ (~optional (~seq #:template template)
                 #:defaults ([template #'_]))
      name:id ...)

   (define (replace name stx)
     (syntax-parse stx
       [ident:id
        #:when (equal? (syntax-e #'ident) '_)
        name]

       [(form ...)
        (datum->syntax this-syntax
                       (map (λ (form)
                              (replace name form))
                            (attribute form))
                       this-syntax)]

       [_ this-syntax]))

   (with-syntax ([(value ...) (map (λ (name)
                                     (replace name #'template))
                                   (attribute name))])
     #'(begin
         (define name (s-lua value))
         ...))])
