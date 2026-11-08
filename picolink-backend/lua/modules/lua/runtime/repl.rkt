#lang picolink/r-lua/base

(require (only lua        loadstring setfenv pcall)
         (only lua/io     stderr read)
         (only lua/debug  traceback)

         (only lua/ext/table   table-ref)
         (only lua/ext/object  send)

         (only r-lua/prelude/binding     let let-values)
         (only r-lua/prelude/condition   unless)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/s-lua       s-lua)
         (only r-lua/prelude/void        void))

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
