#lang picopass

(require (for-syntax racket/base
                     syntax/parse))

(provide (all-defined-out)
         (for-syntax (all-defined-out)))

(define-syntax define-s-lua-forms
  (syntax-parser
    [(_ name ...)
     (with-syntax ([(message ...)
                    (for/list ([name (in-list (attribute name))])
                      (format "cannot use ~a outside of s-lua"
                              (syntax-e name)))])
       #'(begin
           (define-syntax (name stx)
            (error message))
           ...))]))

(define-s-lua-forms
  #%chunk
  #%block
  #%assign
  #%label
  #%break
  #%goto
  #%do
  #%while
  #%repeat
  #%until
  #%if
  #%for
  #%local
  #%then
  #%elseif
  #%else
  #%function
  #%return
  #%member
  #%nil
  #%call
  #%method
  #%table
  #%add
  #%sub
  #%mul
  #%div
  #%exp
  #%mod
  #%cat
  #%lt
  #%le
  #%gt
  #%ge
  #%eq
  #%ne
  #%and
  #%or
  #%neg
  #%not
  #%length)

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

(define-syntax-class vararg
  (pattern (~datum #%vararg)))

; funcname ::=
(define-syntax-class function-name
  (pattern ident:id

           #:with (segments (~optional member))
           (string-split (symbol->string (syntax-e #'ident)) ":")

           #:with (path ...)
           (string-split (syntax-e #'segments) ".")))

[define-language s-lua
 #:entry-point chunk
 #:terminals [name
              boolean
              number
              string
              vararg
              function-name]

 ; chunk ::=
 (chunk
  #:datum-literals [#%chunk]
  (#%chunk ~cut block))

 ; block ::=
 (block
  #:datum-literals [#%block]
  (#%block ~cut
           statement ...
           (~maybe return-statement)))

 ; stat ::=
 (statement
  #:datum-literals [#%assign
                    #%label
                    #%break
                    #%goto
                    #%do
                    #%while
                    #%repeat
                    #%until
                    #%if
                    #%for
                    #%local]

  (#%assign ~cut [var ...] [expr ...])
  (#%label ~cut name)
  (#%break)
  (#%goto ~cut name)
  (#%do ~cut block)
  (#%while ~cut expr block)
  (#%repeat ~cut block (#%until ~cut expr))

  ; if - subforms handle block inlining
  (#%if ~cut expr
        if/then
        if/elseif
        ...
        (~maybe if/else))

  ; for name = exp, exp [, exp]
  (#%for (name expr expr (~maybe expr))
         block)

  ; for-in
  (#%for ([name expr] ...)
         block)

  ; function - subform handles block inlining
  statement/function

  (#%local [var ...+])
  (#%local [var ...+] [expr ...+])

  ; local function - subform handles block inlining
  (#%local statement/function)

  function-call)

 ; if subforms
 (if/then
   #:description "then"
   #:datum-literals [#%then]
   (#%then ~cut block))

 (if/elseif
   #:description "elseif"
   #:datum-literals [#%elseif]
   (#%elseif ~cut expr if/then))

 (if/else
   #:description "else"
   #:datum-literals [#%else]
   (#%else ~cut block))

 ; function subforms
 (statement/function
   #:description "function"
   #:datum-literals [#%function]
   (#%function ~cut (function-name name ... (~maybe vararg))
             block))

 ; retstat ::=
 (return-statement
   #:description "return"
   #:datum-literals [#%return]
   (#%return ~cut expr ...))

 ; varlist - inlined into parent forms

 ; var ::=
 ; . is reserved in racket, replaced with ->
 ; [] has same semantic with different target, replaced with ->
 (var
   #:description "variable"
   #:datum-literals [#%member]
   name
   (#%member prefix-expr name)
   (#%member prefix-expr expr))

 ; namelist - inlined into parent forms

 ; explist - inlined into parent forms

 ; exp ::=
 (expr
   #:description "expression"
   #:datum-literals [#%nil]
   #%nil
   boolean
   number
   string
   vararg
   function-definition
   table
   (unary-op expr)
   (binary-op expr ~cut expr)
   prefix-expr)

 ; prefixexp ::=
 (prefix-expr
   #:description "prefix expression"
   #:datum-literals [quote]
   var
   (quote expr)
   function-call)

 ; functioncall ::=
 (function-call
  #:description "function call"
  #:datum-literals [#%call]
  (#%call prefix-expr-or-method expr ...)
  (#%call prefix-expr-or-method table)
  (#%call prefix-expr-or-method string))

 (prefix-expr-or-method
  #:datum-literals [#%method]
  prefix-expr
  (#%method ~cut prefix-expr name))

 ; args - inlined into function-call

 ; functiondef ::=
 (function-definition
   #:description "function definition"
   #:datum-literals [#%function]
   (#%function ~cut (name ... (~maybe vararg))
               block))

 ; funcbody - inlined into parent forms

 ; parlist - inlined into parent forms

 ; tableconstructor ::=
 (table
  #:datum-literals [#%table]
   (#%table ~cut table-field ...))

 ; fieldlist - inlined into table

 ; field ::=
 (table-field
   #:description "table field"
   [name expr]
   [expr expr]
   expr)

 ; fieldsep - unneeded with s-expressions

 ; binop ::=
 (binary-op
  #:description "binary operator"
  #:datum-literals [#%add
                    #%sub
                    #%mul
                    #%div
                    #%exp
                    #%mod
                    #%cat
                    #%lt
                    #%le
                    #%gt
                    #%ge
                    #%eq
                    #%ne
                    #%and
                    #%or]
  #%add
  #%sub
  #%mul
  #%div
  #%exp
  #%mod
  #%cat
  #%lt
  #%le
  #%gt
  #%ge
  #%eq
  #%ne
  #%and
  #%or)

 ; unop ::=
 (unary-op
  #:description "unary operator"
  #:datum-literals [#%neg
                    #%not
                    #%length]
  #%neg
  #%not
  #%length)]

(define-language-parser parse-s-lua s-lua)
