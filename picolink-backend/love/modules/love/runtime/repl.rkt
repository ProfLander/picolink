#lang picolink/r-lua/base

(require (only lua        loadstring setfenv pcall)
         (only lua/debug  traceback)
         (only lua/io     stderr)

         (only lua/ext/table   table-ref)
         (only lua/ext/object  send)

         (only love/ext/thread  new-thread get-channel)

         (only r-lua/prelude/assignment  set!)
         (only r-lua/prelude/binding     let let-values)
         (only r-lua/prelude/condition   when unless)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/print       print)
         (only r-lua/prelude/s-lua       s-lua)
         (only r-lua/prelude/void        void))

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
