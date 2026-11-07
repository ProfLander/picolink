#lang picolink/r-lua/base

(require r-lua/prelude/sequencing)

(provide (all-defined))

(define-macro let-values
  [(_ _ ...)
   (syntax-parser/scope
    scope
    [(_ ([(name ...) (~cata val r-lua/expr)] ...)
        body ...)

     (for ([name (in-list (apply append (attribute name)))])
       (bind! name (r-lua-var) #:space r-lua-space))

     (with-syntax ([(body ...) (map r-lua/expr (attribute body))])
       (syntax/loc this-syntax
         (#%let-values ([(name ...) val] ...)
                       (#%begin body ...))))])])

(define-macro let
  [(_ ([name val] ...)
      body ...)

   (syntax/loc this-syntax
     (let-values ([(name) val] ...)
       body ...))]

  [(_ name ([ident val] ...)
      body ...)

   (syntax/loc this-syntax
     (letrec ([name (λ (ident ...)
                      body ...)])
       (name val ...)))])

(define-macro letrec-values
  [(_ _ ...)
   (syntax-parser/scope
    scope
    [(_ ([(name ...) val] ...)
        body ...)

     (for ([name (in-list (apply append (attribute name)))])
       (bind! name (r-lua-var) #:space r-lua-space))

     (with-syntax ([(val ...)  (map r-lua/expr (attribute val))]
                   [(body ...) (map r-lua/expr (attribute body))])

       (syntax/loc this-syntax
         (#%letrec-values ([(name ...) val] ...)
                          (#%begin body ...))))])])

(define-macro letrec
  [(_ ([name val] ...)
      body ...)

   (syntax/loc this-syntax
     (letrec-values ([(name) val] ...)
       body ...))])


(define-macro let*
  [(_ (bind ...) body ...)

   (for/foldr ([acc (quasisyntax/loc this-syntax
                      (begin body ...))])

              ([bind (in-list (attribute bind))])

     (quasisyntax/loc acc
       (let (#,bind)
         #,acc)))])
