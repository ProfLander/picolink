#lang racket/base

(require (for-syntax racket/base
                     racket/string

                     syntax/parse

                     picolink2/s-lua/name)

         picolink2/s-lua/ir)

(provide (for-syntax in-s-lua
                     rewrite-name)

         s-lua

         (for-space s-lua

                    #%datum

                    #%chunk
                    #%block

                    #%assign
                    #%local
                    #%label
                    #%goto
                    #%do

                    #%for
                    #%while
                    #%repeat
                    #%break

                    #%if

                    #%function
                    #%vararg
                    #%return
                    #%call

                    #%table
                    #%member
                    #%method

                    #%prefix

                    #%nil

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
                    #%length))

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

(define-for-syntax (local-expand-s-lua stx [ctx #f])
  (local-expand stx 'expression null ctx))

(define-for-syntax (%normalize stx)
  (syntax-parse stx
    #:datum-literals [quote]

    [(quote form)
     (syntax-e #'form)]))

(define-for-syntax (normalize stx)
  (syntax-parse stx
    [(~and ident:id (~not _:s-lua-ir-literal))
     (rewrite-name (symbol->string (syntax-e #'ident)))]

    [_ (%normalize (local-expand-s-lua stx))]))

(define-for-syntax (indent str)
  (string-append "  " (string-replace str "\n" "\n  ")))

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

(define-s-lua-syntax #%chunk
  (syntax-parser
    [(_ block)
     (with-syntax ([lua (normalize #'block)])
       #''lua)]))

(define-s-lua-syntax #%block
  (syntax-parser
    [(_ body ...)
     (with-syntax ([lua (string-join (map normalize (attribute body))
                                     "\n")])
       #''lua)]))

(define-s-lua-syntax #%assign
  (syntax-parser
    [(_ [target:expr ...] [expr:expr ...])

     (let ([targets (map normalize (attribute target))]
           [exprs   (map normalize (attribute expr))])

       (with-syntax

         ([lua (format "~a = ~a"
                       (string-join targets ", ")
                       (string-join exprs   ", "))])

         #''lua))]))

(define-s-lua-syntax #%local
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ [ident:id ...] [expr:expr ...])

     (let ([idents (map normalize (attribute ident))]
           [exprs  (map normalize (attribute expr))])

       (with-syntax

         ([lua (format "local ~a = ~a"
                       (string-join idents ", ")
                       (string-join exprs ", "))])

         #''lua))]

    [(_ [ident:id ...])

     (let ([idents (map normalize (attribute ident))])
       (with-syntax
         ([lua (format "local ~a"

                       (string-join idents ", "))])
         #''lua))]

    [(_ (~and (#%function _ ...)
              function))

     (let ([function (normalize #'function)])
       (with-syntax
         ([lua (format "local ~a" function)])
         #''lua))]))

(define-s-lua-syntax #%label
  (syntax-parser
    [(_ name:id)
     (let ([name (normalize #'name)])
       (with-syntax ([lua (format "::~a::" name)])
         #''lua))]))

(define-s-lua-syntax #%goto
  (syntax-parser
    [(_ name:id)
     (let ([name (normalize #'name)])
       (with-syntax ([lua (format "goto ~a" name)])
         #''lua))]))

(define-s-lua-syntax #%do
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ block)
     (let ([block (normalize #'block)])
       (with-syntax ([lua (string-join

                           (append (list "do")

                                   (if (non-empty-string? block)
                                       (list (indent block))
                                       null)

                                   (list "end"))

                           "\n")])
         #''lua))]))

(define-s-lua-syntax #%for
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ [name ... (#%in expr ...)] block)

     (let* ([name  (map normalize (attribute name))]
            [expr  (map normalize (attribute expr))]
            [block (normalize #'block)])

       (with-syntax ([lua (string-join

                           (append (list
                                    (format "for ~a in ~a do"
                                            (string-join name ", ")
                                            (string-join expr ", ")))

                                   (if (non-empty-string? block)
                                       (list (indent block))
                                       null)

                                   (list "end"))

                           "\n")])
         #''lua))]

    [(_ [name from to (~optional step)] block)

     (let* ([name  (normalize #'name)]
            [from  (normalize #'from)]
            [to    (normalize #'to)]
            [step  (attribute step)]

            [step  (if step
                       (list (normalize step))
                       null)]

            [block (normalize #'block)])

       (with-syntax ([lua (string-join

                           (append (list
                                    (format "for ~a = ~a do"
                                            name
                                            (string-join
                                             (append (list from to)
                                                     step)
                                             ", ")))

                                   (if (non-empty-string? block)
                                       (list (indent block))
                                       null)

                                   (list "end"))

                           "\n")])
         #''lua))]))

(define-s-lua-syntax #%while
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ cond block)

     (let ([cond  (normalize #'cond)]
           [block (normalize #'block)])

       (with-syntax ([lua (string-join

                           (append (list (format "while ~a do" cond))

                                   (if (non-empty-string? block)
                                       (list (indent block))
                                       null)

                                   (list "end"))

                           "\n")])
         #''lua))]))

(define-s-lua-syntax #%repeat
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ block (#%until cond))

     (let ([block (normalize #'block)]
           [cond  (normalize #'cond)])

       (with-syntax ([lua (string-join
                           (append (list "repeat")
                                   (if (non-empty-string? block)
                                       (list (indent block))
                                       null)
                                 (list (format "until ~a" cond)))
                           "\n")])

         #''lua))]))

(define-s-lua-syntax #%break
  (syntax-parser
    [(_)
     #''"break"]))

(define-s-lua-syntax #%if
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ cond
        (#%then then-block)
        (#%elseif elseif-cond (#%then elseif-block)) ...
        (~optional (#%else else-block)))

     (let ([cond          (normalize #'cond)]
           [then-block    (normalize #'then-block)]
           [elseif-conds  (map normalize (attribute elseif-cond))]
           [elseif-blocks (map normalize (attribute elseif-block))]
           [else-cond     (normalize #'else-cond)]
           [else-block    (normalize #'else-block)])

       (with-syntax
         ([lua (string-join

                (append (list (format "if ~a then" cond))

                        (if (non-empty-string? then-block)
                            (list (indent then-block))
                            null)

                        (apply append
                         (for/list ([cond  (in-list elseif-conds)]
                                    [block (in-list elseif-blocks)])

                           (append (list (format "elseif ~a then" cond))

                                   (if (non-empty-string? block)
                                       (list (indent block))
                                       null))))

                        (if else-block
                            (append (list "else")
                                    (if (non-empty-string? else-block)
                                        (list (indent else-block))
                                        null))
                            null)

                        (list "end"))

                "\n")])
         #''lua))]))

(define-s-lua-syntax #%function
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]

    [(_ (~optional name:expr) ((~and arg:id (~not #%vararg))
                               ...
                               (~optional (~and #%vararg vararg)))
        block:expr)

     (let* ([name (attribute name)]
            [name (and name (normalize name))]

            [args (map normalize (attribute arg))]

            [vararg (attribute vararg)]
            [vararg (if vararg
                        (list (normalize vararg))
                        null)]

            [block (normalize #'block)])

       (with-syntax ([lua
                      (string-join
                       (append (list
                                (format "function~a(~a)"
                                        (if name
                                            (format " ~a" name)
                                            "")
                                        (string-join (append args vararg)
                                                     ", ")))

                               (if (non-empty-string? block)
                                   (list (indent block))
                                   null)

                             (list "end"))
                       "\n")])

         #''lua))]))

(define-s-lua-syntax (#%vararg stx)
  #''"...")

(define-s-lua-syntax #%return
  (syntax-parser
    [(_ expr ...)

     (let* ([exprs (map normalize (attribute expr))])

       (with-syntax ([lua (string-join
                           (append (list "return")
                                   (if (pair? exprs)
                                       (list (string-join exprs ", "))
                                       null))
                           " ")])
         #''lua))]))

(define-s-lua-syntax #%call
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ proc (~and arg (#%table _ ...)))

     (let ([proc (normalize #'proc)]
           [arg  (normalize #'arg)])

       (with-syntax
         ([lua (format "~a~a" proc arg)])
         #''lua))]

    [(_ proc (~and arg _:string))

     (let ([proc (normalize #'proc)]
           [arg  (normalize #'arg)])

       (with-syntax
         ([lua (format "~a ~a" proc arg)])
         #''lua))]

    [(_ proc arg ...)
     (let ([proc (normalize #'proc)]

           [args (for/list ([arg (in-list (attribute arg))])
                   (normalize arg))])

       (with-syntax
         ([lua (format "~a(~a)" proc (string-join args ", "))])
         #''lua))]))

(define-s-lua-syntax #%table
  (syntax-parser
    [(_ (~and arg (~or (~and [(~and %key:id
                                    (~bind [key (normalize #'%key)]))
                              (~and %val:expr
                                    (~bind [val (normalize #'%val)]))]
                             (~bind [expr (format "~a = ~a"
                                                  (attribute key)
                                                  (attribute val))]))
                       (~and [(~and %key:expr
                                    (~bind [key (normalize #'%key)]))
                              (~and %val:expr
                                    (~bind [val (normalize #'%val)]))]
                             (~bind [expr (format "[~a] = ~a"
                                                  (attribute key)
                                                  (attribute val))]))
                       (~and %expr:expr
                             (~bind [expr (normalize #'%expr)])))) ...)

     (let ([exprs (attribute expr)])

       (with-syntax
         ([lua (format "{~a}" (string-join exprs ", "))])
         #''lua))]))

(define-s-lua-syntax #%member
  (syntax-parser
    #:literal-sets [s-lua-ir-literals]
    [(_ target:expr name:id)

     (let ([target (normalize #'target)]
           [name   (normalize #'name)])

       (with-syntax ([lua (format "~a.~a" target name)])
         #''lua))]

    [(_ target:expr (#%prefix field:expr))

     (let ([target (normalize #'target)]
           [name   (normalize #'field)])

       (with-syntax ([lua (format "~a[~a]" target name)])
         #''lua))]

    [(_ target:expr name:expr)

     (let ([target (normalize #'target)]
           [name   (normalize #'name)])

       (with-syntax ([lua (format "~a[~a]" target name)])
         #''lua))]))

(define-s-lua-syntax #%method
  (syntax-parser
    [(_ target name)

     (let ([target (normalize #'target)]
           [name   (normalize #'name)])

       (with-syntax ([lua (format "~a:~a" target name)])
         #''lua))]))

(define-s-lua-syntax #%prefix
  (syntax-parser
    [(_ expr:expr)
     (let ([expr (normalize #'expr)])
       (with-syntax ([lua (format "(~a)" expr)])
         #''lua))]))

(define-s-lua-syntax (#%nil stx)
  #''"nil")

(define-for-syntax (make-binop a op b)
  (let ([a (normalize a)]
        [b (normalize b)])
    (with-syntax ([lua (format "~a ~a ~a" a op b)])
      #''lua)))

(define-s-lua-syntax #%add
  (syntax-parser
    [(_ a b)
     (make-binop #'a "+" #'b)]))

(define-s-lua-syntax #%sub
  (syntax-parser
    [(_ a b)
     (make-binop #'a "-" #'b)]))

(define-s-lua-syntax #%mul
  (syntax-parser
    [(_ a b)
     (make-binop #'a "*" #'b)]))

(define-s-lua-syntax #%div
  (syntax-parser
    [(_ a b)
     (make-binop #'a "/" #'b)]))

(define-s-lua-syntax #%exp
  (syntax-parser
    [(_ a b)
     (make-binop #'a "^" #'b)]))

(define-s-lua-syntax #%mod
  (syntax-parser
    [(_ a b)
     (make-binop #'a "%" #'b)]))

(define-s-lua-syntax #%cat
  (syntax-parser
    [(_ a b)
     (make-binop #'a ".." #'b)]))

(define-s-lua-syntax #%lt
  (syntax-parser
    [(_ a b)
     (make-binop #'a "<" #'b)]))

(define-s-lua-syntax #%le
  (syntax-parser
    [(_ a b)
     (make-binop #'a "<=" #'b)]))

(define-s-lua-syntax #%gt
  (syntax-parser
    [(_ a b)
     (make-binop #'a ">" #'b)]))

(define-s-lua-syntax #%ge
  (syntax-parser
    [(_ a b)
     (make-binop #'a ">=" #'b)]))

(define-s-lua-syntax #%eq
  (syntax-parser
    [(_ a b)
     (make-binop #'a "==" #'b)]))

(define-s-lua-syntax #%ne
  (syntax-parser
    [(_ a b)
     (make-binop #'a "~=" #'b)]))

(define-s-lua-syntax #%and
  (syntax-parser
    [(_ a b)
     (make-binop #'a "and" #'b)]))

(define-s-lua-syntax #%or
  (syntax-parser
    [(_ a b)
     (make-binop #'a "or" #'b)]))

(define-for-syntax (make-unop op arg)
  (let ([arg (normalize arg)])
    (with-syntax ([lua (format "~a~a" op arg)])
      #''lua)))

(define-s-lua-syntax #%neg
  (syntax-parser
    [(_ expr:expr)
     (make-unop "-" #'expr)]))

(define-s-lua-syntax #%not
  (syntax-parser
    [(_ expr:expr)
     (make-unop "not " #'expr) ]))

(define-s-lua-syntax #%length
  (syntax-parser
    [(_ expr:expr)
     (make-unop "#" #'expr)]))

(module* test racket/base

  (require racket/string
           rackunit
           (submod ".."))

  (test-equal?
   "assignment"

   (s-lua
    (#%chunk
     (#%block
      (#%assign [x] [y])
      (#%assign [(#%member x 1) (#%member x k)]
                [y z]))))

   (string-join '("x = y"
                  "x[1], x.k = y, z")
                "\n"))

  (test-equal?
   "functioncall"

   (s-lua
    (#%chunk
     (#%block
      (#%call f)
      (#%call f 1 2)
      (#%call f (#%table))
      (#%call f "s")
      (#%call (#%method x f)))))

   (string-join '("f()"
                  "f(1, 2)"
                  "f{}"
                  "f \"s\""
                  "x:f()")
                "\n"))

  (test-equal?
   "label / goto"

   (s-lua
    (#%chunk
     (#%block
      (#%label L)
      (#%goto L))))

   (string-join '("::L::"
                  "goto L")
                "\n"))

  (test-equal?
   "do"

   (s-lua
    (#%chunk
     (#%block
      (#%do (#%block)))))

   (string-join '("do"
                  "end")
                "\n"))

  (test-equal?
   "while"

   (s-lua
    (#%chunk
     (#%block
      (#%while #f
               (#%block
                (#%break))))))

   (string-join '("while false do"
                  "  break"
                  "end")
                "\n"))

  (test-equal?
   "repeat"

   (s-lua
    (#%chunk
     (#%block
      (#%repeat
       (#%block
        (#%break))
       (#%until #t)))))

   (string-join '("repeat"
                  "  break"
                  "until true")
                "\n"))

  (test-equal?
   "if / elseif / else"

   (s-lua
    (#%chunk
     (#%block
      (#%if #t
            (#%then (#%block))
            (#%elseif #f (#%then (#%block)))
            (#%elseif #t (#%then (#%block)))
            (#%else (#%block))))))

   (string-join '("if true then"
                  "elseif false then"
                  "elseif true then"
                  "else"
                  "end")
                "\n"))

  (test-equal?
   "numeric for"

   (s-lua
    (#%chunk
     (#%block
      (#%for [i 1 2]
             (#%block
              (#%break)))
      (#%for [i 1 2 3]
             (#%block
              (#%break))))))

   (string-join '("for i = 1, 2 do"
                  "  break"
                  "end"
                  "for i = 1, 2, 3 do"
                  "  break"
                  "end")
                "\n"))

  (test-equal?
   "generic for"

   (s-lua
    (#%chunk
     (#%block
      (#%for [k v (#%in f g)]
             (#%block
              (#%break))))))

   (string-join '("for k, v in f, g do"
                  "  break"
                  "end")
                "\n"))

  (test-equal?
   "function name body"
   
   (s-lua
    (#%chunk
     (#%block
      (#%function (#%method
                   (#%member
                    (#%member a b)
                    c)
                   d)
                  (x y)
                  (#%block)))))

   (string-join '("function a.b.c:d(x, y)"
                  "end")
                "\n"))

  (test-equal?
   "local function name body"

   (s-lua
    (#%chunk
     (#%block
      (#%local
       (#%function lf (x y)
                   (#%block))))))

   (string-join '("local function lf(x, y)"
                  "end")
                "\n"))

  (test-equal?
   "local namelist"

   (s-lua
    (#%chunk
     (#%block
      (#%local [a b]))))

   (string-join '("local a, b")
                "\n"))

  (test-equal?
   "local namelist = explist"

   (s-lua
    (#%chunk
     (#%block
      (#%local [literals-and-forms]
               [#%nil
                #f
                #t
                1
                "s"
                #%vararg]))))

   (string-join
    '("local literals_and_forms = nil, false, true, 1, \"s\", ...")
    "\n"))

  (test-equal?
   "local functiondef"

   (s-lua
    (#%chunk
     (#%block
      (#%local [functiondef]
               [(#%function () (#%block))]))))

   (string-join '("local functiondef = function()"
                  "end")
                "\n"))

  (test-equal?
   "local prefixexp"

   (s-lua
    (#%chunk
     (#%block
      (#%local [prefixexp]
               [literals-and-forms]))))

   (string-join '("local prefixexp = literals_and_forms")
                "\n"))

  (test-equal?
   "local table"

   (s-lua
    (#%chunk
     (#%block
      (#%local [table]
               [(#%table)]))))

   (string-join '("local table = {}")
                "\n"))

  (test-equal?
   "local binop"

   (s-lua
    (#%chunk
     (#%block
      (#%local [binop]
               [(#%add 1 1)]))))

   (string-join '("local binop = 1 + 1")
                "\n"))

  (test-equal?
   "local unop"

   (s-lua
    (#%chunk
     (#%block
      (#%local [unop]
               [(#%neg 1)]))))

   (string-join '("local unop = -1")
                "\n"))

  (test-equal?
   "prefixexp"

   (s-lua
    (#%chunk
     (#%block
      (#%local [p1] [literals-and-forms])
      (#%local [p2] [(#%call f)])
      (#%local [p3] [(#%prefix literals-and-forms)]))))

   (string-join '("local p1 = literals_and_forms"
                  "local p2 = f()"
                  "local p3 = (literals_and_forms)")
                "\n"))

  (test-equal?
   "var"

   (s-lua
    (#%chunk
     (#%block
      (#%assign [p1] [1])
      (#%assign [(#%member p1 1)] [2])
      (#%assign [(#%member p1 k)] [3]))))

   (string-join '("p1 = 1"
                  "p1[1] = 2"
                  "p1.k = 3")
                "\n"))

  (test-equal?
   "namelist function"

   (s-lua
    (#%chunk
     (#%block
      (#%local (#%function named-varargs (a b #%vararg)
                           (#%block
                            (#%return)))))))

   (string-join '("local function named_varargs(a, b, ...)"
                  "  return"
                  "end")
                "\n"))

  (test-equal?
   "anonymous function"

   (s-lua
    (#%chunk
     (#%block
      (#%local [anonymous]
               [(#%function (a b)
                            (#%block
                             (#%return 1 2)))]))))

   (string-join '("local anonymous = function(a, b)"
                  "  return 1, 2"
                  "end")
                "\n"))

  (test-equal?
   "anonymous vararg function"

   (s-lua
    (#%chunk
     (#%block
      (#%local [anonymous]
               [(#%function (a b #%vararg)
                            (#%block
                             (#%return #%vararg)))]))))

   (string-join '("local anonymous = function(a, b, ...)"
                  "  return ..."
                  "end")
                "\n"))

  (test-equal?
   "table"

   (s-lua
    (#%chunk
     (#%block
      (#%local [t] [(#%table [1 2]
                             [name 3]
                             4
                             [5 6])]))))

   (string-join '("local t = {[1] = 2, name = 3, 4, [5] = 6}")
                "\n"))

  (test-equal?
   "binops"

   (s-lua
    (#%chunk
     (#%block
      (#%local [add] [(#%add 1 1)])
      (#%local [sub] [(#%sub 1 1)])
      (#%local [mul] [(#%mul 1 1)])
      (#%local [div] [(#%div 1 1)])
      (#%local [exp] [(#%exp 1 1)])
      (#%local [mod] [(#%mod 1 1)])
      (#%local [cat] [(#%cat "a" "b")])
      (#%local [lt] [(#%lt 1 2)])
      (#%local [le] [(#%le 1 2)])
      (#%local [gt] [(#%gt 1 2)])
      (#%local [ge] [(#%ge 1 2)])
      (#%local [eq] [(#%eq 1 2)])
      (#%local [ne] [(#%ne 1 2)])
      (#%local [op-and] [(#%and #t #f)])
      (#%local [op-or] [(#%or #t #f)]))))

   (string-join '("local add = 1 + 1"
                  "local sub = 1 - 1"
                  "local mul = 1 * 1"
                  "local div = 1 / 1"
                  "local exp = 1 ^ 1"
                  "local mod = 1 % 1"
                  "local cat = \"a\" .. \"b\""
                  "local lt = 1 < 2"
                  "local le = 1 <= 2"
                  "local gt = 1 > 2"
                  "local ge = 1 >= 2"
                  "local eq = 1 == 2"
                  "local ne = 1 ~= 2"
                  "local op_and = true and false"
                  "local op_or = true or false")
                "\n"))

  (test-equal?
   "unops"

   (s-lua
    (#%chunk
     (#%block
      (#%local [unops] [(#%neg 1)
                        (#%not #t)
                        (#%length "s")]))))

   (string-join '("local unops = -1, not true, #\"s\"")
                "\n"))

  (test-equal?
   "retstat"
   
   (s-lua
    (#%chunk
     (#%block
      (#%function r0 ()
                  (#%block
                   (#%return)))
      (#%function r1 ()
                  (#%block
                   (#%return 1))))))

   (string-join '("function r0()"
                  "  return"
                  "end"
                  "function r1()"
                  "  return 1"
                  "end")
                "\n"))

  (test-equal?
   "vararg-host"

   (s-lua
    (#%chunk
     (#%block
      (#%local (#%function vararg-host (#%vararg)
                           (#%block
                            (#%local [_] [#%vararg])))))))

   (string-join '("local function vararg_host(...)"
                  "  local _sub_ = ..."
                  "end")
                "\n")))
