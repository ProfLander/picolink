#lang picolink/r-lua

(require (only love/runtime/world  genesis quit-with)

         (only love/ext/graphics
               set-color
               rectangle
               graphics-print))

(define-struct dvd-world [logo]
  #:pool-size 2)

(define-struct dvd-logo [x y w h vx vy]
  #:pool-size 2)

(define world
  (dvd-world
   (dvd-logo 20 50
             60 40
              1  1)))

(define (tick-world world)

  (let* ([logo (dvd-world-logo world)]
         [x    (dvd-logo-x logo)]
         [y    (dvd-logo-y logo)]
         [w    (dvd-logo-w logo)]
         [h    (dvd-logo-h logo)]
         [vx   (dvd-logo-vx logo)]
         [vy   (dvd-logo-vy logo)]

         [vx (if (or (<= x   0)
                     (<= 800 (+ x w)))
                 (- vx)
                 vx)]

         [vy (if (or (<= y   0)
                     (<= 600 (+ y h)))
                 (- vy)
                 vy)]

         [x  (+ x vx)]
         [y  (+ y vy)])

    (dvd-world
     (dvd-logo x y
               w h
               vx vy))))

(define (draw-world world)

  (let* ([logo (dvd-world-logo world)]
         [x    (dvd-logo-x logo)]
         [y    (dvd-logo-y logo)]
         [w    (dvd-logo-w logo)]
         [h    (dvd-logo-h logo)])

    (set-color 1 1 1 1)
    (rectangle "fill" x y w h)
    (set-color 0 0 0 1)
    (graphics-print "DVD" (+ x (/ w 4)) (+ y (/ h 4)))))

(define (run world)
  (genesis world
           (on-tick tick-world)
           (on-draw draw-world)))

(run world)
