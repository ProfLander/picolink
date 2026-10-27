#lang picolink/r-lua/base

(provide (all-defined))

(define-macro begin
  [(_ body ...)
   #:when (eq? 'expression (syntax-local-context))

   (with-syntax ([(body ...) (map r-lua/expr (attribute body))])
     (syntax/loc this-syntax
       (#%begin body ...)))])

(define-macro define

  [(_ (name:id arg ...) body ...)
   (syntax/loc this-syntax
     (define name (λ (arg ...) body ...)))]

  [(_ (name:id arg ... . tail) body ...)
   (syntax/loc this-syntax
     (define name (λ (arg ... . tail) body ...)))]

  [(_ (name:id . tail) body ...)
   (syntax/loc this-syntax
     (define name (λ tail body ...)))]

  [(_ name:id expr)
   (bind-in-module! #'name (r-lua-var) #:space r-lua-space)
   (with-syntax ([expr (r-lua/expr #'expr)])
     (syntax/loc this-syntax
       (#%define name expr)))])

(define-macro if
  [(_ (~cata cond r-lua/expr)
      (~cata then r-lua/expr)
      (~cata else r-lua/expr))

   (syntax/loc this-syntax
     (#%if cond then else))])

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

(define-macro set!
  [(_ (~cata name check-var) (~cata val r-lua/expr))
   (syntax/loc this-syntax
     (#%set! name val))])

(define-macro let
  [(_ _ ...)
   (syntax-parser/scope
    scope
    [(_ ([name (~cata val r-lua/expr)] ...)
        body ...)

     (for ([name (in-list (attribute name))])
       (bind! name (r-lua-var) #:space r-lua-space))

     (with-syntax ([(body ...) (map r-lua/expr (attribute body))])
       (syntax/loc this-syntax
         (#%let ([name val] ...)
                (#%begin body ...))))])])

(define-macro letrec
  [(_ _ ...)
   (syntax-parser/scope
    scope
    [(_ ([name val] ...)
        body ...)

     (for ([name (in-list (attribute name))])
       (bind! name (r-lua-var) #:space r-lua-space))

     (with-syntax ([(val ...)  (map r-lua/expr (attribute val))]
                   [(body ...) (map r-lua/expr (attribute body))])
       (syntax/loc this-syntax
         (#%letrec ([name val] ...)
                   (#%begin body ...))))])])
