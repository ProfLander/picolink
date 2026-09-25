#lang racket/base

(require racket/contract
         racket/set

         syntax/parse

         picolink/language
         picolink/binding

         (prefix-in picopass: picolink/s-lua/to-source))

(provide (all-defined-out))

(struct s-lua (body provides)
  #:transparent
  #:methods gen:language
  [(define (source self)
     (with-syntax ([(body ...) (s-lua-body self)])
       #'(#%chunk (#%block body ...))))

   (define (collect-provides self)
     (s-lua-provides self))

   (define (compiler self name)
     (case name
       [(lua) picopass:s-lua->lua]
       [(s-lua) (syntax-parser
                  #:datum-literals [#%chunk #%block]
                  [(#%chunk (#%block body ...))
                   (make-s-lua (attribute body)
                               #:provides (s-lua-provides self))])]
       [else (error "unsupported target language" name)]))])

(define/contract (make-s-lua body #:provides [provides (set)])
  (->* [(listof syntax?)]
       [#:provides (set/c binding?)]
       s-lua?)

  (s-lua body provides))

(define/contract (s-lua-prepend self other)
  (-> s-lua? s-lua? s-lua?)
  (s-lua (append (s-lua-body other) (s-lua-body self))
         (s-lua-provides self)))

(define/contract (s-lua-append self other)
  (-> s-lua? s-lua? s-lua?)
  (s-lua (append (s-lua-body self) (s-lua-body other))
         (s-lua-provides self)))

(define/contract (s-lua-map self f)
  (-> s-lua?
      (-> (listof syntax?) (listof syntax?))
      s-lua?)
  (s-lua (f (s-lua-body self))
         (s-lua-provides self)))
