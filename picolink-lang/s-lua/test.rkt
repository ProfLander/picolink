#lang racket/base

(module+ test
  (require racket/string
           rackunit
           picolink/s-lua/lua
           picolink/s-lua/ir
           (for-template picolink/s-lua/ir))

  (test-equal? "assignment"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%assign [x] [y])
                     (#%assign [(#%member x 1) (#%member x k)]
                               [y z])))))

               (string-join '("x = y"
                              "x[1], x.k = y, z")
                            "\n"))

  (test-equal? "functioncall"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%call f)
                     (#%call f 1 2)
                     (#%call f (#%table))
                     (#%call f "s")
                     (#%call (#%method x f))))))

               (string-join '("f()"
                              "f(1, 2)"
                              "f{}"
                              "f \"s\""
                              "x:f()")
                            "\n"))

  (test-equal? "label / goto"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%label L)
                     (#%goto L)))))

               (string-join '("::L::"
                              "goto L")
                            "\n"))

  (test-equal? "do"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%do (#%block))))))

               (string-join '("do"
                              "end")
                            "\n"))

  (test-equal? "while"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%while #f
                              (#%block
                               (#%break)))))))

               (string-join '("while false do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "repeat"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%repeat
                      (#%block
                       (#%break))
                      (#%until #t))))))

               (string-join '("repeat"
                              "  break"
                              "until true")
                            "\n"))

  (test-equal? "if / elseif / else"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%if #t (#%then (#%block))
                           (#%elseif #f (#%then (#%block)))
                           (#%elseif #t (#%then (#%block)))
                           (#%else (#%block)))))))

               (string-join '("if true then"
                              "elseif false then"
                              "elseif true then"
                              "else"
                              "end")
                            "\n"))

  (test-equal? "numeric for"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%for [i 1 2]
                            (#%block
                             (#%break)))
                     (#%for [i 1 2 3]
                            (#%block
                             (#%break)))))))

               (string-join '("for i = 1, 2 do"
                              "  break"
                              "end"
                              "for i = 1, 2, 3 do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "generic for"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%for ([k f]
                             [v g])
                            (#%block
                             (#%break)))))))

               (string-join '("for k, v in f, g do"
                              "  break"
                              "end")
                            "\n"))

  (test-equal? "function name body"
               
               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%function a.b.c:d (x y)
                                 (#%block))))))

               (string-join '("function a.b.c:d(x, y)"
                              "end")
                            "\n"))

  (test-equal? "local function name body"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local
                      (#%function lf (x y)
                                  (#%block)))))))

               (string-join '("local function lf(x, y)"
                              "end")
                            "\n"))

  (test-equal? "local namelist"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [a b])))))

               (string-join '("local a, b")
                            "\n"))

  (test-equal? "local namelist = explist"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [literals-and-forms]
                              [#%nil
                               #f
                               #t
                               1
                               "s"
                               #%vararg])))))

               (string-join '("local literals__minus__and__minus__forms = nil, false, true, 1, \"s\", ...")
                            "\n"))

  (test-equal? "local functiondef"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [functiondef]
                              [(#%function () (#%block))])))))

               (string-join '("local functiondef = function()"
                              "end")
                            "\n"))

  (test-equal? "local prefixexp"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [prefixexp]
                              [literals-and-forms])))))

               (string-join '("local prefixexp = literals__minus__and__minus__forms")
                            "\n"))

  (test-equal? "local table"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [table]
                              [(#%table)])))))

               (string-join '("local table = {}")
                            "\n"))

  (test-equal? "local binop"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [binop]
                              [(#%add 1 1)])))))

               (string-join '("local binop = 1 + 1")
                            "\n"))

  (test-equal? "local unop"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [unop]
                              [(#%neg 1)])))))

               (string-join '("local unop = -1")
                            "\n"))

  (test-equal? "prefixexp"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [p1] [literals-and-forms])
                     (#%local [p2] [(#%call f)])
                     (#%local [p3] ['literals-and-forms])))))

               (string-join '("local p1 = literals__minus__and__minus__forms"
                              "local p2 = f()"
                              "local p3 = (literals__minus__and__minus__forms)")
                            "\n"))

  (test-equal? "var"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%assign [p1] [1])
                     (#%assign [(#%member p1 1)] [2])
                     (#%assign [(#%member p1 k)] [3])))))

               (string-join '("p1 = 1"
                              "p1[1] = 2"
                              "p1.k = 3")
                            "\n"))

  (test-equal? "namelist function"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local (#%function named-varargs (a b #%vararg)
                                          (#%block
                                           (#%return))))))))

               (string-join '("local function named__minus__varargs(a, b, ...)"
                              "  return"
                              "end")
                            "\n"))

  (test-equal? "anonymous function"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [anonymous]
                              [(#%function (a b)
                                           (#%block
                                            (#%return 1 2)))])))))

               (string-join '("local anonymous = function(a, b)"
                              "  return 1, 2"
                              "end")
                            "\n"))

  (test-equal? "anonymous vararg function"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [anonymous]
                              [(#%function (a b #%vararg)
                                           (#%block
                                            (#%return #%vararg)))])))))

               (string-join '("local anonymous = function(a, b, ...)"
                              "  return ..."
                              "end")
                            "\n"))

  (test-equal? "table"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [t] [(#%table [1 2]
                                            [name 3]
                                            4
                                            [5 6])])))))

               (string-join '("local t = {[1] = 2, name = 3, 4, [5] = 6}")
                            "\n"))

  (test-equal? "binops"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
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
                     (#%local [op-or] [(#%or #t #f)])))))

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
                              "local op__minus__and = true and false"
                              "local op__minus__or = true or false")
                            "\n"))

  (test-equal? "unops"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local [unops] [(#%neg 1)
                                       (#%not #t)
                                       (#%length "s")])))))

               (string-join '("local unops = -1, not true, #\"s\"")
                            "\n"))

  (test-equal? "retstat"
               
               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%function r0 ()
                                 (#%block
                                  (#%return)))
                     (#%function r1 ()
                                 (#%block
                                  (#%return 1)))))))

               (string-join '("function r0()"
                              "  return"
                              "end"
                              "function r1()"
                              "  return 1"
                              "end")
                            "\n"))

  (test-equal? "vararg-host"

               (compile/chunk
                (in-s-lua
                 #'(#%chunk
                    (#%block
                     (#%local (#%function vararg-host (#%vararg)
                                          (#%block
                                           (#%local [_] [#%vararg]))))))))

               (string-join '("local function vararg__minus__host(...)"
                              "  local _ = ..."
                              "end")
                            "\n")))
