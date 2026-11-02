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

         (only lua/ext/lctx
               lctx)

         r-lua/prelude/binding
         r-lua/prelude/conditional
         r-lua/prelude/print
         r-lua/prelude/surface
         r-lua/prelude/void)

(provide (all-defined))

(define (repl scope)

  (let ([input (read)])
    (let-values ([(res err) (loadstring input "repl")])

      (if res

          (let ([f (setfenv res (table-ref lctx scope))])
            (let-values ([(res err) (pcall f)])
              (unless res
                (send stderr "write" (traceback err 2)))))

          (send stderr "write" (traceback err 2))))

    (repl scope)))
