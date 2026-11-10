#lang picolink/r-lua

(require (only lua/ext/table table-set!)

         (rename love/ext/graphics print graphics-print)

         (only love/runtime/world  genesis quit-with))

(define world "hello, world")

(define (draw-world world)
  (graphics-print world 30 30))

(define (ping world)

  (let ([tick (λ (world)

                (let ([world (<> world "!")])

                  (if (< 100 (length world))
                      (quit-with world)
                      world)))])

    (genesis world
             (on-tick tick)
             (on-draw draw-world))))

(define (pong world)

  (let ([tick (λ (world)

                (let ([world (substring world
                                        0
                                        (- (length world) 1))])

                  (if (string-empty? world)
                      (quit-with world)
                      world)))])

    (genesis world
             (on-tick tick)
             (on-draw draw-world))))

#;(letrec ([loop-a (λ (world)
                     (loop-b (ping world)))]
           [loop-b (λ (world)
                     (loop-a (pong world)))])
    (loop-a world))

(pong (ping world))
