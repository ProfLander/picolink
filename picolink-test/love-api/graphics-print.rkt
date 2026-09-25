#lang picolink/lambda

(require (in lua print)
         (in love/graphics [print graphics-print])
         (in love/ext
             set-love-draw
             set-love-update
             set-love-key-pressed))

(set-love-draw
 (λ ()
   (graphics-print "love draw" 400 300)))

(set-love-update
 (λ (dt)
   (print "love update" dt)))

(set-love-key-pressed
 (λ (key)
   (print "key pressed" key)))
