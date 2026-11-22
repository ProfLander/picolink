#lang picolink/r-lua/base

(require (only lua        load-string set-fenv! pcall)
         (only lua/io     stderr read)
         (only lua/debug  traceback)

         (only lua/ext/table   table-ref)
         (only lua/ext/ops     :)

         (only r-lua/prelude/binding     let let-values)
         (only r-lua/prelude/condition   unless)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/if          if)
         (only r-lua/prelude/s-lua       s-lua))

(provide (all-defined))

(define (repl scope)

  (let ([input (read)])
    (let-values ([(res err) (load-string input "repl")])

      (if res

          (let ([fenv (table-ref (s-lua scopes) scope)]
                [f (set-fenv! res fenv)])
            (let-values ([(res err) (pcall f)])
              (unless res
                (: stderr "write" (traceback err)))))

          (: stderr "write" (traceback err))))

    (repl scope)))
