#lang picolink/r-lua/base

(require (only lua/ext/table  table-set!)

         (only lua/runtime     lua-pre)

         (only love/ext        love arg timer load)
         (only love/ext/timer  step)
         (only love/ext/arg    parse-game-arguments)

         (only love/runtime/error-handler  error-handler)
         (only love/runtime/repl           repl-init)

         (only r-lua/prelude/condition   when)
         (only r-lua/prelude/definition  define)
         (only r-lua/prelude/function    thunk const))

(provide (all-defined))

(define (love-run entry-point)

  (lua-pre)
  (repl-init)

  (table-set! love "errorhandler" error-handler)

  (table-set! love "run"
              (thunk
               (when load
                 (load (parse-game-arguments arg) arg))

               (when timer
                 (step))

               entry-point)))
