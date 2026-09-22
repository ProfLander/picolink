#lang racket/base

(require racket/contract
         racket/list

         syntax/parse
         syntax/id-table
         syntax/id-set

         picolink/input
         (only-in picolink/output
                  make-output))

(provide (all-defined-out))

(define (compile/s-lua stx)

  (define (parse stx)
    (syntax-parse stx
      #:datum-literals [require in provide define λ]

      [(require (in _:id _:id ...) ...) #f]
      [(provide _:id ...) #f]

      [(define name:id (~and %val:expr (~parse val (parse #'%val))))
       #'(#%local [name] [val])]

      [(λ (arg:id ...)
         (~and %body:expr (~parse body (parse #'%body))) ...
         (~and %ret:expr (~parse ret (parse #'%ret))))
       #'(#%function (arg ...)
                     (#%block body ... (#%return ret)))]

      [((~and %proc:expr
              (~parse proc (parse #'%proc)))
        (~and %arg:expr
              (~parse arg (parse #'%arg)))
        ...)
       #'(proc arg ...)]

      [_:id
       this-syntax]))

  (with-syntax ([(body ...) (filter-map parse (syntax-e stx))])
    (make-output 's-lua
                 #'(#%chunk
                    (#%block
                     body ...)))))

(struct input/lambda (source)
  #:transparent
  #:methods gen:input

  [(define (source self)
     (input/lambda-source self))

   (define (collect-require self stx)
     (syntax-parse stx
       #:datum-literals [require in]
       [(require (in mod:id req:id ...) ...)
        (cons this-syntax
              (for/fold ([acc (make-immutable-free-id-table)])
                        ([entry (in-list (syntax-e
                                          #'((mod req ...) ...)))])
                (syntax-parse entry
                  [(mod:id req:id ...)
                   (free-id-table-set acc #'mod
                                      (immutable-free-id-set
                                       (syntax-e #'(req ...))))])))]
       [_ #f]))

   (define (collect-provide self stx)
     (syntax-parse stx
       #:datum-literals [provide]
       [(provide prov:id ...)
        (immutable-free-id-set (attribute prov))]
       [_ #f]))

   (define (compiler _self name)
     (case name
       [(s-lua) compile/s-lua]
       [else (error "unsupported backend" name)]))])

(define/contract (make-input/lambda stx)
  (-> syntax? input/lambda?)
  (input/lambda stx))

(register-input 'lambda make-input/lambda)
