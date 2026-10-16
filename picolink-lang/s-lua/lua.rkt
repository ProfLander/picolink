#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)

         racket/string

         syntax/parse

         picolink/syntax

         (for-template racket/base
                       picolink/s-lua/ir))

(provide (all-defined-out))

(define current-separator (make-parameter "\n"))

(define reserved-symbols
  (list '=
        '::
        '+
        '-
        '*
        '/
        '^
        '%
        '|.|
        '..
        '<
        '<=
        '>
        '>=
        '==
        '~=))

(define reserved-names
  (list '#%return
        '#%vararg
        '#%method

        'break
        'goto
        'do
        'while
        'repeat
        'until
        'if
        'for
        'function
        'local
        'then
        'elseif
        'else
        'return
        'nil
        'false
        'true
        'and
        'or
        'not
        'length))

(define-syntax-class name
  (pattern ident:id
           #:attr symbol (syntax-e #'ident)
           #:attr string (symbol->string (attribute symbol))

           #:do [(define reserved-name
                   (or (member (attribute symbol) reserved-names)
                       (member (attribute symbol) reserved-symbols)))]

           #:fail-when reserved-name
           (format "reserved name: ~a" (car reserved-name))))

(define-syntax-class function-name
  (pattern ident:id

           #:with (segments (~optional member))
           (string-split (symbol->string (syntax-e #'ident)) ":")

           #:with (path ...)
           (string-split (syntax-e #'segments) ".")))

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

(define reserved-identifier->lua-name
  (hash
   "function" "__function__"))

(define (rewrite-name ident #:omit [omit null])
  (syntax-parse ident
    [ident:id
     (let* ([id (symbol->string (syntax-e #'ident))]

            [id (for/fold ([acc id])
                          ([(from to) (in-hash
                                       reserved-symbol->lua-name)])
                  (if (member from omit)
                      acc
                      (string-replace acc from to)))]
            
            [id (for/fold ([acc id])
                          ([(from to) (in-hash
                                       reserved-identifier->lua-name)])
                  (if (equal? acc from)
                      to
                      acc))])
       id)]))

(define (indent str)
  (string-replace str "\n" "\n  "))

(define (format-block block)
  (if (string=? "" block)
      block
      (format "~a~a" (current-separator) block)))

(define (format-body stmts ret
                     #:leading-newline [leading-newline #t])
  (string-join (append (if (and leading-newline
                                (or (pair? stmts)
                                    ret))
                           (list "")
                           null)
                       stmts
                       (if ret
                           (list ret)
                           null))
               (current-separator)))

(define compile/chunk
  (syntax-parser
    [chunk:chunk
     (attribute chunk.lua)]))

(define-syntax define-lua-syntax-class
  (syntax-parser
    [(_ name body ...)
     #'(define-parser-like-syntax-class name lua
         body ...)]))

(define-lua-syntax-class chunk
  #:literal-sets [s-lua-ir-literals]

  [(#%chunk ~! block:block)
   (attribute block.lua)])

(define-lua-syntax-class block
  #:literal-sets [s-lua-ir-literals]

  [(#%block ~! stmt:statement ...
            (~optional ret:return-statement))
   (format-body (attribute stmt.lua)
                (attribute ret.lua)
                #:leading-newline #f)])

(define-lua-syntax-class statement
  #:literal-sets [s-lua-ir-literals]

  [(#%assign ~! [var:var ...] [exp:expr ...])
   (format "~a = ~a"
           (string-join (attribute var.lua) ", ")
           (string-join (attribute exp.lua) ", "))]

  [(#%break)
   "break"]

  [(#%label ~! name:name)
   (format "::~a::" (rewrite-name #'name))]

  [(#%goto ~! name:name)
   (format "goto ~a" (rewrite-name #'name))]

  [(#%do ~! b:block)
   (format "do~a~aend"
           (indent (format-block (attribute b.lua)))
           (current-separator))]

  [(#%while ~! cond:expr
            b:block)
   (format "while ~a do~a~aend"
           (attribute cond.lua)
           (indent (format-block (attribute b.lua)))
           (current-separator))]

  [(#%repeat ~! b:block
             (#%until cond:expr))
   (format "repeat~a~auntil ~a"
           (indent (format-block (attribute b.lua)))
           (current-separator)
           (attribute cond.lua))]

  [(#%if ~! cond:expr
         then:if/then
         elseif:if/elseif
         ...
         (~optional else:if/else))
   (let ([separator (current-separator)])
     (format "if ~a ~a~a~a~aend"
             (attribute cond.lua)
             (attribute then.lua)
             (string-join (append (if (pair? (attribute elseif))
                                      (list "")
                                      null)
                                  (attribute elseif.lua))
                          separator)
             (if (attribute else)
                 (string-append separator (attribute else.lua))
                 "")
             separator))]

  [(#%for (name:name from:expr
                     to:expr
                     (~optional step:expr))
          ~!
          b:block)
   (format "for ~a = ~a, ~a~a do~a~aend"
           (rewrite-name #'name)
           (attribute from.lua)
           (attribute to.lua)
           (if (attribute step)
               (string-append ", " (attribute step.lua))
               "")
           (indent (format-block (attribute b.lua)))
           (current-separator))]

  [(#%for ([name:name exp:expr] ...)
          ~!
          b:block)
   (format "for ~a in ~a do~a~aend"
           (string-join (map rewrite-name (attribute name)) ", ")
           (string-join (attribute exp.lua) ", ")
           (indent (format-block (attribute b.lua)))
           (current-separator))]

  [stat:statement/function
   (attribute stat.lua)]

  [(#%local [var:var ...+] ~! (~optional [exp:expr ...+]))

   (if (attribute exp)

       (format "local ~a = ~a"
               (string-join (attribute var.lua) ", ")
               (string-join (attribute exp.lua) ", "))

       (format "local ~a"
               (string-join (attribute var.lua) ", ")))]

  [(#%local func:statement/function)
   (format "local ~a" (attribute func.lua))]

  [call:function-call
   (attribute call.lua)])

(define-lua-syntax-class if/then
  #:literal-sets [s-lua-ir-literals]

  [(#%then ~! b:block)
   (format "then~a"
           (indent (format-block (attribute b.lua))))])

(define-lua-syntax-class if/elseif
  #:literal-sets [s-lua-ir-literals]

  [(#%elseif ~! exp:expr then:if/then)
   (format "elseif ~a ~a"
           (attribute exp.lua)
           (attribute then.lua))])

(define-lua-syntax-class if/else
  #:literal-sets [s-lua-ir-literals]

  [(#%else ~! b:block)
   (format "else~a"
           (indent (format-block (attribute b.lua))))])

(define-lua-syntax-class statement/function
  #:literal-sets [s-lua-ir-literals]

  [(#%function ~! (name:function-name
                   arg:name ...
                   (~optional (~and #%vararg
                                    (~bind [vararg #t]))))
               b:block)
   (format "function ~a(~a)~a~aend"
           (rewrite-name #'name #:omit '("." ":"))
           (string-join (append (map (compose symbol->string
                                              syntax-e)
                                     (attribute arg))
                                (if (attribute vararg)
                                    (list "...")
                                    null))
                        ", ")
           (indent (format-block (attribute b.lua)))
           (current-separator))])

(define-lua-syntax-class return-statement
  #:literal-sets [s-lua-ir-literals]

  [(#%return ~! expr:expr ...)
   (format "return~a~a"
           (if (pair? (attribute expr))
               " "
               "")
           (string-join (attribute expr.lua) ", "))])

(define-lua-syntax-class var
  #:literal-sets [s-lua-ir-literals]

  [name:name
   (rewrite-name #'name)]

  [(#%member exp:expr field:name)
   (format "~a.~a" (attribute exp.lua) (rewrite-name #'field))]

  [(#%member exp:prefix-expr field:expr)
   (format "~a[~a]" (attribute exp.lua) (attribute field.lua))])

(define-lua-syntax-class expr
  #:literal-sets [s-lua-ir-literals]

  [#%nil
   "nil"]

  [b:boolean
   (case (syntax-e #'b)
     [(#t) "true"]
     [(#f) "false"])]

  [num:number
   (number->string (syntax-e #'num))]

  [str:string
   (format "\"~a\"" (syntax-e #'str))]

  [#%vararg
   "..."]

  [func:function-definition
   (attribute func.lua)]

  [tbl:table
   (attribute tbl.lua)]

  [(unop:unary-op ~! exp:expr)
   (let ([op-sym (attribute unop.lua)])
     (format "~a~a~a"
             op-sym
             (if (equal? "not" op-sym)
                 " "
                 "")
             (attribute exp.lua)))]

  [(binop:binary-op ~! a:expr b:expr)
   (format "~a ~a ~a"
           (attribute a.lua)
           (attribute binop.lua)
           (attribute b.lua))]

  [prefix:prefix-expr
   (attribute prefix.lua)])

(define-lua-syntax-class prefix-expr
  #:literal-sets [s-lua-ir-literals]
  #:literals [quote]

  [var:var
   (attribute var.lua)]

  [(quote ~! exp:expr)
   (format "(~a)" (attribute exp.lua))]

  [call:function-call
   (attribute call.lua)])

(define-lua-syntax-class function-call
  #:literal-sets [s-lua-ir-literals]

  [(#%call prefix:prefix-expr-or-method str:string)
   (format "~a \"~a\""
           (attribute prefix.lua)
           (syntax-e #'str))]

  [(#%call prefix:prefix-expr-or-method
           table:table)
   (format "~a~a"
           (attribute prefix.lua)
           (attribute table.lua))]

  [(#%call prefix:prefix-expr-or-method
           exp:expr ...)
   (format "~a(~a)"
           (attribute prefix.lua)
           (string-join (attribute exp.lua) ", "))])

(define-lua-syntax-class prefix-expr-or-method
  #:literal-sets [s-lua-ir-literals]

  [(#%method ~! prefix:prefix-expr name:name)
   (format "~a:~a"
           (attribute prefix.lua)
           (rewrite-name #'name))]

  [prefix:prefix-expr
   (attribute prefix.lua)])

(define-lua-syntax-class function-definition
  #:literal-sets [s-lua-ir-literals]

  [(#%function ~! (arg:name ... (~optional
                                 (~and #%vararg
                                       (~bind [vararg #t]))))
               b:block)

   (format "function(~a)~a~aend"
           (string-join (append (map rewrite-name (attribute arg))
                                (if (attribute vararg)
                                    (list "...")
                                    null))
                        ", ")
           (indent (format-block (attribute b.lua)))
           (current-separator))])

(define-lua-syntax-class table
  #:literal-sets [s-lua-ir-literals]

  [(#%table ~! field:table-field ...)
   (format "{~a}" (string-join (attribute field.lua) ", "))])

(define-lua-syntax-class table-field
  #:literal-sets [s-lua-ir-literals]

  [(key:name ~! exp:expr)
   (format "~a = ~a"
           (rewrite-name #'key)
           (attribute exp.lua))]

  [(key:expr ~! exp:expr)
   (format "[~a] = ~a"
           (attribute key.lua)
           (attribute exp.lua))]

  [exp:expr
   (attribute exp.lua)])

(define-lua-syntax-class binary-op
  #:literal-sets [s-lua-ir-literals]

  [#%add "+"]
  [#%sub "-"]
  [#%mul "*"]
  [#%div "/"]
  [#%exp "^"]
  [#%mod "%"]
  [#%cat ".."]
  [#%lt "<"]
  [#%le "<="]
  [#%gt ">"]
  [#%ge ">="]
  [#%eq "=="]
  [#%ne "~="]
  [#%and "and"]
  [#%or "or"])

(define-lua-syntax-class unary-op
  #:literal-sets [s-lua-ir-literals]

  [#%neg "-"]
  [#%not "not"]
  [#%length "#"])
