#lang picolink/backend

(require syntax/parse

         picolink/lua
         picolink/module/compiled
         (prefix-in s-lua: picolink/s-lua)
         (for-template picolink/love/run
                       picolink/s-lua/ir))

#:target       love-target
#:search-paths love-search-paths
#:transform    love-transform
#:link         love-link
#:run          love-run
#:interact     love-interact

(define love-target    lua-target)

(define (love-search-paths)
  (append (lua-search-paths)
          (list 'picolink/love/modules)))

(define love-transform lua-transform)

(define (lift-love-runner mod)
  (compiled-module-update
   mod
   #:stx (syntax-parse (compiled-module-stx mod)
           #:literal-sets [s-lua-ir-literals]

           [(#%chunk (#%block body ...))
            (in-s-lua
             #'(#%chunk
                (#%block
                 (#%local [runner]
                          [(#%call require "love.runtime.runner")])

                 (#%local [love__minus__run]
                          [(#%member runner love__minus__run)])

                 (#%call love__minus__run
                         (#%function
                          ()
                          (#%block
                           body ...))))))])))

(define (love-transform-module _lctx prog mod)
  (let* ([is-entry-point (equal? (syntax-e (compiled-module-ident mod))
                                 'entry-point)]
         [mod (if is-entry-point
                  (lift-love-runner mod)
                  mod)]
         [mod (splice-module-name mod)]
         [mod (splice-requires mod)]
         [mod (splice-lctx mod)]
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
