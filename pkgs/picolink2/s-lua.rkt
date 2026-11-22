#lang racket/base

(require (for-syntax racket/base
                     racket/function
                     racket/string

                     syntax/parse

                     picolink2/s-lua/name))

(provide (for-syntax in-s-lua
                     rewrite-name)

         s-lua

         (for-space s-lua
                    #%datum
                    #%assign
                    #%local
                    #%function
                    #%call
                    #%do))

; utilities

(define-for-syntax in-s-lua
  (let ([intro (make-interned-syntax-introducer 's-lua)])
    (λ (stx)
      (intro stx 'add))))

(define-syntax s-lua
  (syntax-parser
    [(_ body ...)
     (in-s-lua
      (syntax/loc this-syntax
        (begin
          body ...)))]))

(define-syntax define-s-lua-syntax
  (syntax-parser
    [(_ name transformer)
     (with-syntax ([name* (in-s-lua #'name)])
       (syntax/loc this-syntax
         (define-syntax name* transformer)))]))

(define-for-syntax normalize
  (syntax-parser
    #:datum-literals [quote]

    [ident:id
     (rewrite-name (symbol->string (syntax-e #'ident)))]

    [(quote form)
     (syntax-e #'form)]))

; s-lua IR -> lua string

(define-s-lua-syntax #%datum
  (syntax-parser
    [(_ . #f)
     #''"false"]

    [(_ . #t)
     #''"true"]

    [(_ . n:number)
     (with-syntax ([n (number->string (syntax-e #'n))])
       #''n)]

    [(_ . s:string)
     (with-syntax ([s (format "\"~a\"" (syntax-e #'s))])
       #''s)]))

(define-s-lua-syntax #%assign
  (syntax-parser
    [(_ [ident:id ...] [e:expr ...])

     (with-syntax
       ([lua (format "~a = ~a"
                     (string-join (map normalize (attribute ident)) ", ")
                     (string-join (map normalize (attribute e)) ", "))])
       #''lua)]))

(define-s-lua-syntax #%local
  (syntax-parser
    [(_ [ident:id ...] [e:expr ...])

     (with-syntax
       ([lua (format "local ~a = ~a"
                     (string-join (map normalize (attribute ident)) ", ")
                     (string-join (map normalize (attribute e)) ", "))])
       #''lua)]))

(define-s-lua-syntax #%function
  (syntax-parser
    #:datum-literals [#%block]

    [(_ (arg:id ...) (#%block body ... ret))

     (with-syntax
       ([lua
         (format "function(~a)\n  ~a\nend"
                 (string-join (map (compose symbol->string syntax-e)
                                   (attribute arg))
                              ", ")
                 (string-join (append (map normalize (attribute body))
                                      (list (format "  return ~a"
                                                    (normalize #'ret))))
                              "\n"))])
       #''lua)]))

(define-s-lua-syntax #%call
  (syntax-parser
    [(_ proc arg ...)

     (with-syntax
       ([lua (format "~a(~a)"
                     (normalize #'proc)
                     (string-join (map normalize (attribute arg))
                                  ", "))])
       #''lua)]))

(define-s-lua-syntax #%do
  (syntax-parser
    #:datum-literals [#%block]

    [(_ (#%block body ...))

     (let ([bodies (map (curryr local-expand 'expression null)
                        (attribute body))])

       (with-syntax ([lua (format "do\n  ~a\nend"
                                  (string-join (map normalize bodies)
                                               "\n"))])
         #''lua))]))
