#lang picolink/backend

(require racket/syntax

         syntax/parse

         picolink/lua
         picolink/module/compiled
         (prefix-in s-lua: picolink/s-lua)
         (only-in picolink/s-lua/lua rewrite-name)

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

(define (love-transform-module _lctx prog mod)
  (let* ([is-entry-point (equal? (syntax-e (compiled-module-ident mod))
                                 'entry-point)]
         [mod (splice-requires mod)]
         [mod (splice-lctx mod)]
         [mod (if is-entry-point
                  (lift-love-runtime mod)
                  mod)]
         [mod (splice-module-name mod)]
         [mod (splice-provides mod)])

    mod))

(define love-link      lua-link)

(define love-run       lua-run)

(define love-interact  lua-interact)

(current-lua-executable "love")
(current-lua-build-subdir "love")
(current-lua-linker lua-link/collection)
(current-lua-runner #'love-run)
(current-lua-entry-point-name "main")
(current-lua-transform-module love-transform-module)
