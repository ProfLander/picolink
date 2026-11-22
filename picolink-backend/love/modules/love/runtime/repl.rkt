#lang picolink/r-lua/base

(require (only lua        load-string set-fenv! pcall)
         (only lua/debug  traceback)
         (only lua/io     stderr)

         (only lua/ext/table   table-ref)
         (only lua/ext/ops     :)

         (only love/ext/thread  new-thread get-channel)

         (only r-lua/prelude/assignment  set!)
         (only r-lua/prelude/binding     let let-values)
         (only r-lua/prelude/condition   when unless)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/s-lua       s-lua))

(provide repl-init
         repl-read)

(define stdin-thread  #f)
(define stdin-channel #f)

(define repl-scope "entry-point")

(define (repl-init)
  (set! stdin-thread  (new-thread  "love/runtime/stdin-thread.lua"))
  (set! stdin-channel (get-channel "stdin"))
  (: stdin-thread "start"))

(define (repl-read)
  (let ([input (: stdin-channel "pop")])
    (when input
      (let-values ([(res err) (load-string input "repl")])
        (if res
            (let ([fenv (table-ref (s-lua scopes) repl-scope)]
                  [f (set-fenv! res fenv)])
              (let-values ([(res err) (pcall f)])
                (unless res
                  (: stderr "write" (traceback err)))))
            (: stderr "write" err))))))
