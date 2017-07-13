(require (lib "fungraph.ss" "concabs"))
(load "quilting.scm")

(define turn
  (lambda (img times)
    (if (= times 0)
        img
        (turn (quarter-turn-right img)
              (- times 1)))))

(define half-turn
  (lambda (img)
    (turn img 2)))

(define quarter-turn-left
  (lambda (img)
    (turn img 3)))

(define side-by-side
  (lambda (img-l img-r)
    (quarter-turn-left
     (stack (quarter-turn-right img-l)
            (quarter-turn-right img-r)))))

(define pinwheel
  (lambda (img)
    (define top
      (side-by-side (quarter-turn-right img)
                    (half-turn img)))
    (define bottom
      (side-by-side img
                    (quarter-turn-left img)))
    (stack top bottom)))

(define rcross-bb2
    (overlay (filled-triangle -1 1 1 -1 1 1)
             (filled-triangle -1/2 1/2 -1/2 -1/2 1/2 -1/2)))

(define one-seventy-three
  (- (* 5 5 7)
     2))

(define avg
    (lambda (x y)
      (* 1/2 (+ x y))))

(define ladder-height
  (lambda (ladder-length base-distance)
    (- (sqrt ladder-length)
       (sqrt base-distance))))