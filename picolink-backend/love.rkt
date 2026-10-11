#lang picolink/backend

(require picolink/lua
         picolink/module/compiled
         (prefix-in s-lua: picolink/s-lua)
         (for-template picolink/love/run
                       picolink/s-lua/ir))

#:target    love-target
#:modules   love-modules
#:transform love-transform
#:link      love-link
#:run       love-run
#:interact  love-interact

(define love-target    lua-target)

(define love-modules   lua-modules)

(define (love-transform lctx prog)
  (let ([prog (lua-transform lctx prog)])
    (cons (make-repl-module) prog)))

(define (love-transform-module _lctx prog mod)
  (let* ([is-entry-point (equal? (syntax-e (compiled-module-ident mod))
                                 'entry-point)]
         [mod (rewrite-intrinsics prog mod)]
         [mod (splice-requires+provides mod)]
         [mod (if is-entry-point
                  (splice-repl-thread
                   (splice-unbuffered-stdio mod))
                  mod)])

    mod))

(define (make-repl-module)
  (make-compiled-module
   #'repl
   #:language s-lua:#%language
   #:require-scope
   (syntax-local-make-definition-context)
   #:module-scope
   (syntax-local-make-definition-context)
   #:provide-scope
   (syntax-local-make-definition-context)
   #:stx
   stx-repl-module))

(define (splice-repl-thread mod)
  (compiled-module-update
   mod
   #:stx
   (s-lua-append (compiled-module-stx mod)
                 stx-repl-thread)))

(define stx-repl-thread
  #'(#%chunk
     (#%block

      (#%local [repl-thread repl-channel])

      (#%function (love.load)

                  (#%block
                   (#%assign [repl-thread]
                             [(#%call
                               (#%member (#%member love thread)
                                         newThread)
                               "repl.lua")])

                   (#%assign [repl-channel]
                             [(#%call
                               (#%member (#%member love thread)
                                         getChannel)
                               "repl")])

                   (#%call (#%method repl-thread start))))

      (#%function (love.update dt)

                  (#%block
                   (#%local [input]
                            [(#%call (#%method repl-channel
                                               pop))])

                   (#%if input
                         (#%then
                          (#%block
                           (#%local [res err] [(#%call loadstring
                                                       input
                                                       "repl")])

                           (#%if res

                                 (#%then
                                  (#%block
                                   (#%call res)))

                                 (#%else
                                  (#%block
                                   (#%call
                                    (#%method (#%member io stderr)
                                              write)
                                    err))))))))))))

(define stx-repl-module
  #'(#%chunk
     (#%block

      (#%local [channel] [(#%call (#%member (#%member love thread)
                                            getChannel)
                                  "repl")])

      (#%call print channel)

      (#%while #t

               (#%block
                (#%local [input] [(#%call (#%member io read))])
                (#%call (#%method channel push) input))))))

(define love-link      lua-link)

(define love-run       lua-run)

(define love-interact  lua-interact)

(current-lua-executable "love")
(current-lua-build-subdir "love")
(current-lua-linker lua-link/collection)
(current-lua-runner #'love-run)
(current-lua-entry-point-name "main")
(current-lua-transform-module love-transform-module)
