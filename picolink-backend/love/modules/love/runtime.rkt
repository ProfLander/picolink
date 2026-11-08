#lang picolink/r-lua/base

(require (only lua/ext/table  table-set!)

         (only lua/runtime/io  set-unbuffered-stdio!)

         (only love/ext        love arg timer load)
         (only love/ext/timer  step)
         (only love/ext/arg    parse-game-arguments)

         (only love/runtime/repl  repl-init)

         (only r-lua/prelude/condition   when)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/function    thunk)
         (only r-lua/prelude/logic       or))

(provide (all-defined))

(define (love-run entry-point)

  (set-unbuffered-stdio!)
  (repl-init)

  (table-set! love "run"
              (thunk
                (when load
                  (load (parse-game-arguments arg) arg))

                (when timer
                  (step))

                (thunk
                  (or (entry-point) 0)))))
