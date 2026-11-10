#lang picolink/r-lua/base

(provide (all-defined))

(define-macro λ
  [(_ _ ...)
   (syntax-parser/scope
    scope
    [(_ args body ...)

     (define parse-args
       (syntax-parser
         [[(~datum :) arg:id tail-bind:id]
          (let ([arg (bind! #'arg (r-lua-var) #:space r-lua-space)])
            (with-syntax ([arg #`[: #,arg tail-bind]])
              (cons #'arg
                    null)))]

         [arg:id
          (cons (list (bind! #'arg (r-lua-var) #:space r-lua-space))
                null)]

         [(arg:id ... [opt-arg opt-val] ...
                  . (~or [(~datum :) tail:id tail-bind:id]
                         tail:id))

          (with-syntax ([tail-bind (if (attribute tail-bind)
                                       (attribute tail-bind)
                                       #'list)])
            (let* ([ord-args
                    (for/list ([arg (in-list (attribute arg))])
                      (bind! arg (r-lua-var)
                             #:space r-lua-space))]

                   [opt-args
                    (for/list ([arg (in-list (attribute opt-arg))])
                      (bind! arg (r-lua-var)
                             #:space r-lua-space))]

                   [tail
                    (bind! #'tail (r-lua-var)
                           #:space r-lua-space)]

                   [args #`(#,@ord-args
                            #,@opt-args
                            . [: #,tail tail-bind])])

              (cons args
                    #'([(opt-arg) (#%if (#%s-lua (#%eq opt-arg nil))
                                        opt-val
                                        opt-arg)]
                       ...))))]
         
         [(arg:id ... [opt-arg:id opt-val] ...)

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
