#lang picolink/lambda

(require (in lua print)
         (in love/graphics [print graphics-print])
         (in love
             draw
             update
             [keypressed key-pressed]))

(set! draw
      (λ ()
        (graphics-print "love draw" 400 300)))

(set! update
      (λ (dt)
        (print "love update" dt)))

(set! key-pressed
      (λ (key)
        (print "key pressed" key)))
