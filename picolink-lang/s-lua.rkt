#lang racket/base

(require racket/contract
         racket/struct

         syntax/parse

         picolink/language
         picolink/binding

         picolink/s-lua/to-source)

(provide (all-defined-out))

(struct s-lua (body provides)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (λ (self) 's-lua)
      (λ (self) (list (cons 'provides (s-lua-provides self))
                      (list 'body
                            (syntax->datum
                             (s-lua-body self)))))))]

  #:methods gen:language
  [(define (source self)
     (s-lua-body self))

   (define (provides self)
     (s-lua-provides self))

   (define (compiler self name)
     (case name
       [(lua) s-lua->lua]
       [(s-lua) (λ (stx)
                  (make-s-lua stx
                              #:provides (s-lua-provides self)))]
       [else (error "unsupported target language" name)]))])

(define/contract (make-s-lua body #:provides [provides null])
  (->* [syntax?]
       [#:provides (listof binding?)]
       s-lua?)

  (s-lua body (make-module-provides provides)))

(define/contract (s-lua-prepend self other)
  (-> s-lua? s-lua? s-lua?)
  (s-lua (syntax-parse (s-lua-body self)
           #:datum-literals [#%chunk #%block]
           [(#%chunk (#%block body-self ...))
            (syntax-parse (s-lua-body other)
              #:datum-literals [#%chunk #%block]
              [(#%chunk (#%block body-other ...))
               #'(#%chunk (#%block body-other ... body-self ...))])])
         (s-lua-provides self)))

(define/contract (s-lua-append self other)
  (-> s-lua? s-lua? s-lua?)
  (s-lua (syntax-parse (s-lua-body self)
           #:datum-literals [#%chunk #%block]
           [(#%chunk (#%block body-self ...))
            (syntax-parse (s-lua-body other)
              #:datum-literals [#%chunk #%block]
              [(#%chunk (#%block body-other ...))
               #'(#%chunk (#%block body-self ... body-other ...))])])
         (s-lua-provides self)))

(define/contract (s-lua-map self f)
  (-> s-lua?
      (-> (listof syntax?) (listof syntax?))
      s-lua?)
  (s-lua (syntax-parse (s-lua-body self)
           #:datum-literals [#%chunk #%block]
           [(#%chunk (#%block body ...))
            (with-syntax ([(body ...) (f (attribute body))])
              #'(#%chunk (#%block body ...)))])
         (s-lua-provides self)))
