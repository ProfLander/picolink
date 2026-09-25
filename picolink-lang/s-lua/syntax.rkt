#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         picolink/s-lua/grammar)

(provide (all-defined-out))

(define-syntax chunk
  (syntax-parser
    [(_ block)
     #'(#%chunk block)]))

(define-syntax block
  (syntax-parser
    [(_ stmt ...)
     #'(#%block stmt ...)]))

(begin-for-syntax
  (define-syntax-class set-spec
    (pattern [ident:id val:expr])
    (pattern ident:id
             #:attr val #'#%nil)))

(define-syntax set-values!
  (syntax-parser
    [(_ (spec:set-spec ...))
     #'(#%assign [spec.ident ...]
                 [(~? spec.val) ...])]))

(define-syntax label
  (syntax-parser
    [(_ name:id)
     #'(#%label name)]))

(define-syntax break
  (syntax-parser
    [(_)
     #'(#%break)]))

(define-syntax goto
  (syntax-parser
    [(_ name:id)
     #'(#%goto name)]))

(define-syntax do
  (syntax-parser
    [(_ stmt:expr ...)
     #'(#%do
        (#%block stmt ...))]))

(define-syntax while
  (syntax-parser
    [(_ cond:expr stmt:expr ...)
     #'(#%while cond
        (#%block stmt ...))]))

(define-syntax repeat
  (syntax-parser
    #:datum-literals [until]
    [(_ stmt:expr ... (until cond:expr))
     #'(#%repeat (#%block stmt ...)
                 (#%until cond))]))

(define-syntax if
  (syntax-parser
    [(_ [cond:expr then:expr]
        [(~and (~not (~datum else))
               elsecond:expr)
         elsethen:expr]
        ...
        (~optional [(~datum else) else:expr]))
     #'(#%if cond
             (#%then then)
             (#%elseif elsecond elsethen)
             ...
             (~? (#%else else)))]))

(define-syntax for
  (syntax-parser
    [(_ ([name:id exp:expr] ...)
        body:expr ...)
     #'(#%for ([name exp] ...)
              (#%block body ...))]

    [(_ [name:id from:expr to:expr (~optional step:expr)]
        body:expr ...)
     #'(#%for (name from to (~? step))
              (#%block body ...))]))

(define-syntax local
  (syntax-parser
    [(_ (spec:set-spec ...))
     #'(#%local [spec.ident ...]
                [(~? spec.val) ...])]

    [(_ (name:id arg:id ...) body:expr ...)
     #'(#%local (#%function (name arg ...)
                            body ...))]))

(define-syntax function
  (syntax-parser
    [(_ (name:id arg:id ...) body ...)
     #'(#%function (name arg ...)
                   body ...)]))

(define-syntax return
  (syntax-parser
    [(_ val:expr ...)
     #'(#%return val ...)]))

(define-syntax ->
  (syntax-parser
    [(_ target:expr field:expr ...)
     (for/fold ([acc #'target])
               ([field (in-list (attribute field))])
       #`(#%member #,acc #,field))]))

(define-syntax #%app
  (syntax-parser
    [(_ proc:expr arg:expr ...)
     #'(#%call proc arg ...)]))

(define-syntax =>
  (syntax-parser
    [(_ target:expr method:id)
     #`(#%method target method)]))

(define-syntax table
  (syntax-parser
    [(_ (~seq key:expr val:expr) ...)
     #`(#%table (~@ key val) ...)]))

(define-syntax define-binops
  (syntax-parser
    [(_ [name:id stx:id] ...)
     (with-syntax ([ooo (quote-syntax ...)])
       #'(begin
           (define-syntax name
             (syntax-parser
               [(_ head:expr tail:expr ooo)
                (for/fold ([acc #'head])
                          ([tail (in-list (attribute tail))])
                  #`(#%call stx #,acc #,tail))]))
           ...))]))

(define-binops
  [+   #%add]
  [*   #%mul]
  [/   #%div]
  [^   #%exp]
  [%   #%mod]
  [..  #%cat]
  [<   #%lt]
  [<=  #%le]
  [>   #%gt]
  [>=  #%ge]
  [==  #%eq]
  [~=  #%ne]
  [and #%and]
  [or  #%or])

(define-syntax -
  (syntax-parser
    [(_ head:expr)
     #`(#%neg head)]

    [(_ head:expr tail:expr ...)
     (for/fold ([acc #'head])
               ([tail (in-list (attribute tail))])
       #`(#%sub #,acc #,tail))]))

(define-syntax not
  (syntax-parser
    [(_ head:expr)
     #`(#%call #%not head)]))

(define-syntax length
  (syntax-parser
    [(_ head:expr)
     #`(#%call #%length head)]))
