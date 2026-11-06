#lang picolink/backend

(require racket/set
         racket/function
         racket/syntax

         syntax/parse

         picolink/lua
         picolink/module/compiled
         (prefix-in s-lua: picolink/s-lua)

         (for-template picolink/love/run
                       picolink/s-lua/ir))

#:target       love-target
#:search-paths love-search-paths
#:modules      love-modules
#:transform    love-transform
#:link         love-link
#:run          love-run
#:interact     love-interact

(define love-target lua-target)

(define (love-search-paths)
  (append (lua-search-paths)
          (list 'picolink/love/modules)))

(define (love-modules)
  (append (lua-modules)
          (list #'love/runtime
                #'love/runtime/stdin-thread)))

(define love-transform lua-transform)

(define (lift-love-runtime mod)
  (compiled-module-update
   mod
   #:stx (syntax-parse (compiled-module-stx mod)
           #:literal-sets [s-lua-ir-literals]

           [(#%chunk (#%block body ... tail))

            (in-s-lua
             (with-syntax ([tail (syntax-parse #'tail
                                   #:literal-sets [s-lua-ir-literals]
                                   [(#%call print tail)
                                    #'tail]
                                   [_ this-syntax])])
               #'(#%chunk
                  (#%block
                   (#%local [runtime]
                            [(#%call require "love.runtime")])

                   (#%call (#%member runtime love-run)
                           (#%function
                            ()
                            (#%block
                             body ...
                             (#%return tail))))))))])))

(define love-link      lua-link)

(define love-run       lua-run)

(define love-interact  lua-interact)

(current-lua-executable "love")
(current-lua-build-subdir "love")
(current-lua-linker lua-link/collection)
(current-lua-runner #'love-run)
(current-lua-entry-point-name "main")
(current-lua-transform-module-body-pre identity)
(current-lua-transform-module-body-post lift-love-runtime)
