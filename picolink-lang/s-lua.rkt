#lang racket/base

(require racket/contract
         picolink/language
         (prefix-in picopass: picopass/lang/s-lua/to-source))

(provide (all-defined-out))

(struct s-lua (body)
  #:transparent
  #:methods gen:language
  [(define (source self)
     (with-syntax ([(body ...) (s-lua-body self)])
       #'(#%chunk (#%block body ...))))

   (define (compiler self name)
     (case name
       [(lua) picopass:s-lua->lua]
       [else (error "unsupported target language" name)]))])

(define/contract (make-s-lua body)
  (-> (listof syntax?) s-lua?)
  (s-lua body))

(define/contract (s-lua-prepend self other)
  (-> s-lua? s-lua? s-lua?)
  (s-lua (append (s-lua-body other) (s-lua-body self))))

(define/contract (s-lua-append self other)
  (-> s-lua? s-lua? s-lua?)
  (s-lua (append (s-lua-body self) (s-lua-body other))))

(define/contract (s-lua-map self f)
  (-> s-lua?
      (-> (listof syntax?) (listof syntax?))
      s-lua?)
  (s-lua (f (s-lua-body self))))
