#lang picolink/r-lua/base

(require (only lua
               loadstring
               setfenv
               pcall)

         (only lua/debug
               traceback)

         (only lua/io
               stderr)

         (only lua/ext/table
               table-ref)

         (only lua/ext/object
               send)

         (only love/ext/thread
               new-thread
               get-channel)

         r-lua/prelude/surface
         r-lua/prelude/conditional
         r-lua/prelude/print
         r-lua/prelude/void)

(provide repl-init
         repl-read)

(define stdin-thread  #f)
(define stdin-channel #f)

(define repl-scope "entry-point")

(define (repl-init)
  (set! stdin-thread  (new-thread  "love/runtime/stdin-thread.lua"))
  (set! stdin-channel (get-channel "stdin"))
  (send stdin-thread "start"))

(define (repl-read)
  (let ([input (send stdin-channel "pop")])
    (when input
      (let-values ([(res err) (loadstring input "repl")])
        (if res
            (let ([f (setfenv res (table-ref (s-lua lctx) repl-scope))])
              (let-values ([(res err) (pcall f)])
                (unless res
                  (send stderr "write" (traceback err)))))
            (send stderr "write" err))))))
