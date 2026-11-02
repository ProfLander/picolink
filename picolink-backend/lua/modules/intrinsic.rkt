#lang picolink/r-lua/base

(require r-lua/prelude/surface)

(provide (all-defined)
         (all-from r-lua/prelude/surface))

(define-macro define-intrinsics
  [(_ (~optional (~seq #:template template)
                 #:defaults ([template #'_]))
      (~or name:id
           [from-name:id to-name:id])
      ...)

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

   (with-syntax ([([name value] ...)
                  (append (for/list ([name (attribute name)])
                            (list name (replace name #'template)))

                          (for/list ([from-name (attribute from-name)]
                                     [to-name   (attribute to-name)])
                            (list to-name
                                  (replace from-name #'template))))])
     #'(begin
         (define name (s-lua value))
         ...))])
