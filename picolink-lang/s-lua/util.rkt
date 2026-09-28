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
