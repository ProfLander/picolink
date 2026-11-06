#lang picolink/language

(require racket/base
         racket/function
         racket/match
         syntax/parse

         (prefix-in s-lua: picolink/s-lua/lua)
         (for-template picolink/s-lua/ir))

(provide (all-defined-out))

#:extract  s-lua-extract
#:wrap     s-lua-wrap
#:parse    s-lua-parse
#:compiler s-lua-compiler

(define s-lua-extract (const null))

(define (s-lua-wrap stx)
  (in-s-lua
   (with-syntax ([(body ...) stx])
     #'(#%chunk (#%block body ...)))))

(define s-lua-parse identity)

(define s-lua-compiler 
  (match-λ
   ['s-lua identity]
   ['lua s-lua:compile/chunk]
   [target (error "unsupported backend" target)]))

(define (%s-lua-append a b)
  (syntax-parse a
    #:literal-sets [s-lua-ir-literals]
    [(#%chunk (#%block (~and body-a (~not (#%return _))) ...
                       (~optional (#%return ret-a))))

     (syntax-parse b
       #:literal-sets [s-lua-ir-literals]
       [(#%chunk (#%block (~and body-b (~not (#%return _))) ...
                          (~optional (#%return ret-b))))
        (when (and (attribute ret-a)
                   (attribute ret-b))
          (error "cannot append s-lua chunks with multiple returns"))

        (in-s-lua
         (with-syntax ([(ret ...) (cond
                                    [(attribute ret-a)
                                     (list #'(#%return ret-a))]
                                    [(attribute ret-b)
                                     (list #'(#%return ret-b))]
                                    [else null])])
           #'(#%chunk
              (#%block
               body-a ...
               body-b ...
               ret ...))))])]))

(define (s-lua-append a . bs)
  (if (pair? bs)
      (%s-lua-append a (apply s-lua-append bs))
      a))


