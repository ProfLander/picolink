#lang picolink/r-lua/base

(require prelude/surface)

(provide (all-defined))

(define foldl-values
  (s-lua
   (#%function (f #%vararg)
               (#%block
                (#%local [acc] [(#%call select 1 #%vararg)])
                (#%for (i 2 (#%call select "#" #%vararg))
                       (#%block
                        (#%assign [acc]
                                  [(#%call f
                                           acc
                                           '(#%call select
                                                    i
                                                    #%vararg))])))
                (#%return acc)))))

(define foldr-values
  (s-lua
   (#%function (f #%vararg)
               (#%block
                (#%local [count] [(#%call select "#" #%vararg)])
                (#%local [acc]   [(#%call select 1 #%vararg)])
                (#%for (i count 2 -1)
                       (#%block
                        (#%assign [acc]
                                  [(#%call f
                                           '(#%call select i #%vararg)
                                           acc)])))
                (#%return acc)))))
