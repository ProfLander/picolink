#lang picolink/r-lua/base

(require (only lua/ext/table
               table-set!)

         (only lua/runtime/io
               set-unbuffered-stdio!)

         (only love/ext

               love
               arg

               timer

               load)

         (only love/ext/timer
               step)

         (only love/ext/arg
               parse-game-arguments)

         (only love/runtime/repl
               repl-init)

         r-lua/prelude/condition
         r-lua/prelude/definition
         r-lua/prelude/function
         r-lua/prelude/logic
         r-lua/prelude/print
         r-lua/prelude/void)

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
