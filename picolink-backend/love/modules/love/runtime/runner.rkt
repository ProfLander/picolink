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

         r-lua/prelude/conditional
         r-lua/prelude/logic
         r-lua/prelude/surface
         r-lua/prelude/void)

(provide (all-defined))

(define (love-run entry-point)
  (table-set! love "run"
              (λ ()

                (set-unbuffered-stdio!)
                (repl-init)

                (when load
                  (load (parse-game-arguments arg) arg))

                (when timer
                  (step))

                (λ ()
                  (or (entry-point) 0)))))
