#lang picolink/r-lua/base

(require (only lua
               loadstring
               setfenv
               pcall)

         (only lua/io stderr read)

         (only lua/debug traceback)

         (only lua/ext/table
               table-ref)

         (only lua/ext/object
               send)

         r-lua/prelude/binding
         r-lua/prelude/condition
         r-lua/prelude/definition
         r-lua/prelude/if
         r-lua/prelude/print
         r-lua/prelude/s-lua
         r-lua/prelude/void)

(provide (all-defined))

(define (repl scope)

  (let ([input (read)])
    (let-values ([(res err) (loadstring input "repl")])

      (if res

          (let ([f (setfenv res (table-ref (s-lua lctx) scope))])
            (let-values ([(res err) (pcall f)])
              (unless res
                (send stderr "write" (traceback err 2)))))

          (send stderr "write" (traceback err 2))))

    (repl scope)))
