#lang picolink/r-lua

(for ([i (in-naturals)]
      [j (in-list (list 3 2 1))])
  (print (format "%s: %s" i j)))

(print "")

(for/fold ([acc null])
          ([i (in-range 1 6)]
           [j (in-range 5 12 2)])
  (cons (cons i j) acc))

(print "")

(for/list ([i (in-range 1 6)]
           [j (in-range 2 12 2)])
  (cons i j))
