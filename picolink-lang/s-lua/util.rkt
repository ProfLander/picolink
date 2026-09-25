#lang racket/base

(require racket/string
         syntax/parse)

(provide (all-defined-out))

(define reserved-symbol->lua-name
  (hash
   "="  "__eq__"
   ":" "__colon__"
   "::" "__2colon__"
   "+"  "__plus__"
   "-"  "_"
   "*"  "__star__"
   "/"  "__slash__"
   "^"  "__caret__"
   "%"  "__percent__"
   "#"  "__hash__"
   "."  "__dot__"
   ".." "__2dot__"
   "<"  "__lt__"
   "<=" "__le__"
   ">"  "__gt__"
   ">=" "__ge__"
   "==" "__2eq__"
   "~=" "__ne__"))

(define (rewrite-name ident #:omit [omit null])
  (syntax-parse ident
    (ident:id
     (with-syntax ([new-ident
                    (string->symbol
                     (for/fold ([acc (symbol->string (syntax-e #'ident))])
                               ([(from to) (in-hash
                                            reserved-symbol->lua-name)])
                       (if (member from omit)
                           acc
                           (string-replace acc from to))))])
       (syntax/loc #'ident
         new-ident)))))

(define (splice-requires requires body)

  (define module-requires
    (for/list ([pair (in-hash-keys requires)])
      (list (string->symbol (cdr pair)) (car pair))))

  (define member-requires
    (for/list ([(pair reqs) (in-hash requires)])
      (let ([name (string->symbol (cdr pair))])
        (map (lambda (req)
               (list name req))
             reqs))))

  (define module-binding
    (if (pair? module-requires)
        (with-syntax ([([require-target require-target-str] ...)
                       module-requires])
          #'[(#%local [require-target ...]
                      [(require require-target-str) ...])])
        #'[]))

  (define member-binding
    (if (pair? member-requires)
        (with-syntax ([([[require-id-target require-id] ...] ...)
                       member-requires])
          #'[(#%local [require-id ... ...]
                      [(#%member require-id-target require-id) ... ...])])
        #'[]))

  (syntax-parse body
    [((~datum #%chunk)
      ((~datum #%block)
       body ...))
     (with-syntax
       ([(module-binding ...) module-binding]
        [(member-binding ...) member-binding])

       #'(#%chunk
          (#%block
           module-binding ...
           member-binding ...
           body ...)))]))

(define (splice-provides provides body)
  "Splice PROVIDES into BODY as a return table"

  (syntax-parse body
    [((~datum #%chunk)
      ((~datum #%block)
       body ...))
     (with-syntax
       ([(module-provide ...) provides])

       #'(#%chunk
          (#%block

           body ...

           (#%return (#%table
                      [module-provide module-provide]
                      ...)))))]))

(define (make-package-preloader name body)
  "Return an S-Lua statement that injects BODY into package.preload
   as a module with NAME"

  (syntax-parse body
    [((~datum #%chunk)
      ((~datum #%block) body ...))
     #`(#%assign [(#%member (#%member package preload)
                            #,name)]
                 [(#%function (#%vararg)
                              (#%block
                               body ...))])]))

