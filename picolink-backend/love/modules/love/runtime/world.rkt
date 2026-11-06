#lang picolink/r-lua/base

(require (only lua
               collectgarbage
               tostring
               error)

         (only lua/coroutine
               yield)

         (only lua/ext/table
               table-ref
               table-set!)

         (only lua/string
               format)

         (only love/ext
               timer
               event
               graphics)

         (only love/ext/timer
               step
               sleep)

         (only love/ext/event
               pump
               poll)

         (only love/ext/graphics
               is-active
               origin
               clear
               get-background-color
               present)

         (only love/runtime/repl
               repl-read)

         r-lua/prelude/binding
         r-lua/prelude/conditional
         r-lua/prelude/logic
         r-lua/prelude/print
         r-lua/prelude/struct
         r-lua/prelude/surface)

(provide quit-with
         %love-runner
         %love-runner-handle-events
         %love-runner-update
         %love-runner-draw
         genesis)

(define-struct quit-with [world])

(define (quit-with-world self)
  (unless (quit-with? self)
    (error (format "%s is not a quit-with" self)))
  (quit-with-ref self 0))

(define (update-with-then world f)
  (if (quit-with? world)
      (quit-with-world world)
      (f world)))

(define (%love-runner-handle-events world handlers)

  (letrec ([handle-events
            (λ (world handlers iter)

              (let-values ([(name a b c d e f) (iter)])

                (if name

                    (let* ([handler (table-ref handlers name)]
                           [result (if handler
                                       (handler world a b c d e f)
                                       world)])

                      (if (quit-with? result)
                          result
                          (handle-events result handlers iter)))

                    world)))])

    (if event

        (begin
          (pump)
          (handle-events world handlers (poll)))

        world)))

(define (%love-runner-update world handlers)

  (let ([dt (if timer (step) 0)]
        [update (table-ref handlers "update")])

    (if update
        (update world dt)
        world)))

(define (%love-runner-draw world handlers)
  (when (and graphics (is-active))
    (origin)
    (clear (get-background-color))

    (let ([draw (table-ref handlers "draw")])
      (when draw
        (draw world)))

    (present)))

(define (%love-runner world handlers)

  (table-set! handlers "lowmemory"
              (or (table-ref handlers "lowmemory")
                  (λ (world)
                    (collectgarbage)
                    (collectgarbage)
                    world)))

  (table-set! handlers "threaderror"
              (or (table-ref handlers "threaderror")
                  (λ (_world t err)
                    (error (format "thread error (%s)\n\n%s"
                                   (tostring t)
                                   err)))))

  (table-set! handlers "quit"
              (or (table-ref handlers "quit")
                  quit-with))

  (letrec ([runner
            (λ (world handlers)

              (repl-read)

              (update-with-then
               (%love-runner-handle-events world handlers)

               (λ (world)
                 (update-with-then
                  (%love-runner-update world handlers)

                  (λ (world)
                    (%love-runner-draw world handlers)

                    (when timer
                      (sleep 0.001))

                    (yield)

                    (runner world handlers))))))])

    (runner world handlers)))

(define-macro define-love-runner
  [(_ name
      [handler-ident %handler-key
                     (~optional (~and #:required (~parse %required #'handler-ident)))]
      ...)

   (define required (filter identity (attribute %required)))

   (with-syntax ([(handler-expr ...)
                  (for/list ([ident (in-list (attribute handler-ident))])
                    (format-id ident "~a-expr" ident))]

                 [(handler-key ...)
                  (for/list ([ident (in-list (attribute handler-ident))])
                    (format-id ident "~a-key" ident))]

                 [(required-expr ...)
                  (for/list ([required (in-list required)])
                    (format-id required "~a-expr" required))]

                 [(required-error ...)
                  (for/list ([required (in-list required)])
                    (format "must provide a ~a clause"
                            required))])

     #'(define-macro name
         [(_ state-expr
             (~or (~optional (~and ((~datum handler-ident) handler-expr)
                                   (~parse handler-key #'%handler-key)))
                  ...)
             (... ...))

          (~@ #:fail-unless (attribute required-expr)
              required-error)
          ...

          #'(let (((... ~?) [handler-ident handler-expr])
                  ...)
              (%love-runner
               state-expr
               (s-lua (#%table ((... ~?) [handler-key handler-ident])
                               ...))))]))])

(define-love-runner genesis

  [on-tick             update]

  [on-focus            focus]
  [on-visible          visible]
  [on-resize           resize]
  [on-display-rotate   displayrotated]

  [on-key-press        keypressed]
  [on-key-release      keyreleased]

  [on-mouse-press      mousepressed]
  [on-mouse-release    mousereleased]
  [on-mouse-move       movemoved]
  [on-mouse-wheel      wheelmoved]
  [on-mouse-focus      mousefocus]

  [on-touch-press      touchpressed]
  [on-touch-release    touchreleased]
  [on-touch-move       touchmoved]

  [on-joystick-added   joystickadded]
  [on-joystick-removed joystickremoved]
  [on-joystick-press   joystickpressed]
  [on-joystick-release joystickreleased]
  [on-joystick-axis    joystickaxis]
  [on-joystick-hat     joystickhat]

  [on-gamepad-press    gamepadpressed]
  [on-gamepad-release  gamepadreleased]
  [on-gamepad-axis     gamepadaxis]

  [on-text-input       textinput]
  [on-text-edit        textedited]

  [on-drop-file        filedropped]
  [on-drop-directory   directorydropped]

  [on-error            errorhandler]
  [on-thread-error     threaderror]
  [on-low-memory       lowmemory]
  [on-quit             quit]

  [on-draw             draw]

  [stop-with           stop-with]
  [stop-when           stop-when]

  [check-with          check-with])
