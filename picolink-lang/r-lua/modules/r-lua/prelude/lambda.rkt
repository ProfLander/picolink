#lang picolink/r-lua/base

(provide (all-defined))

(define-macro λ
  [(_ _ ...)
   (syntax-parser/scope
    scope
    [(_ args body ...)

     (with-syntax* ([args
                     (syntax-parse #'args
                       [arg:id
                        (bind! #'arg (r-lua-var) #:space r-lua-space)]

                       [(arg:id ... . tail:id)

                        (let ([args
                               (for/list ([arg (in-list (attribute arg))])
                                 (bind! arg (r-lua-var)
                                        #:space r-lua-space))]

                              [tail
                               (bind! #'tail (r-lua-var)
                                      #:space r-lua-space)])

                          (append args tail))]
                       
                       [(arg:id ...)

                        (for/list ([arg (in-list (attribute arg))])
                          (bind! arg (r-lua-var) #:space r-lua-space))])]

                    [(body ...) (map r-lua/expr (attribute body))])

       (syntax/loc this-syntax
         (#%abs args
                (#%begin body ...))))])])

(define-macro lambda
  [(_ form ...)
   (syntax/loc this-syntax
     (λ form ...))])

