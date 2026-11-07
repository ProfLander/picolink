#lang picolink/r-lua/base

(provide (all-defined))

(define-macro λ
  [(_ _ ...)
   (syntax-parser/scope
    scope
    [(_ args body ...)

     (define parse-args
       (syntax-parser
         [arg:id
          (cons (list (bind! #'arg (r-lua-var) #:space r-lua-space))
                null)]

         [(arg:id ... [opt-arg opt-val] ... . tail:id)

          (let ([args
                 (for/list ([arg (in-list (attribute arg))])
                   (bind! arg (r-lua-var)
                          #:space r-lua-space))]

                [opt-args
                 (for/list ([arg (in-list (attribute opt-arg))])
                   (bind! arg (r-lua-var)
                          #:space r-lua-space))]

                [tail
                 (bind! #'tail (r-lua-var)
                        #:space r-lua-space)])

            (cons (append args opt-args tail)
                  #'([(opt-arg) (#%if (#%s-lua (#%eq opt-arg nil))
                                      opt-val
                                      opt-arg)]
                     ...)))]
         
         [(arg:id ... [opt-arg opt-val] ...)

          (let ([args (for/list ([arg (in-list (attribute arg))])
                        (bind! arg (r-lua-var) #:space r-lua-space))]
                [opt-args (for/list ([arg (in-list (attribute opt-arg))])
                            (bind! arg (r-lua-var) #:space r-lua-space))])

            (cons (append args opt-args)
                  #'([(opt-arg) (#%if (#%s-lua (#%eq opt-arg nil))
                                      opt-val
                                      opt-arg)]
                     ...)))]))

     (with-syntax* ([(args . (bind ...)) (parse-args #'args)]
                    [(body ...) (map r-lua/expr (attribute body))])

       (syntax/loc this-syntax
         (#%abs args
                (#%let-values (bind ...)
                              (#%begin body ...)))))])])

(define-macro lambda
  [(_ form ...)
   (syntax/loc this-syntax
     (λ form ...))])

