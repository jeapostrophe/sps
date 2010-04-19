#lang scheme/base
(require 2htdp/universe
         scheme/match
         "sps.ss")

(define ship-rad 1.0)
(define speed 5.0)
(define bullet-rad (/ ship-rad 4.0))
(define screen-scale 5.0)
(define screen-width (* screen-scale 16))
(define screen-height (* screen-scale 9))

(define scale 10)
(define ship (circle (* ship-rad scale) 'solid "red"))
(define bullet (circle (* bullet-rad scale) 'solid "black"))

(define the-ship
  (make-body 'ship (vector 5.0 5.0) ship-rad (vector 0.0 0.0)))

(define the-simulation
  (create-simulation))

(simulation-add-body! the-simulation the-ship)

(define bullet-density 4.0)
(define (add-fresh-bullets!)
  (printf "Adding bullets!~n")
  (for ([i (in-range 0.0 (/ screen-width bullet-density))])
    (simulation-add-body! 
     the-simulation
     (make-body 'bullet (vector (* bullet-density i) screen-height) bullet-rad (vector 0.0 (* -1 speed))))))

(define dead? #f)
(define (collide! b1 b2)
  (printf "Collide! ~S ~S~n" b1 b2)
  (set! dead? #t))

(define tick-rate (exact->inexact 1/30))

(define-struct screen (time))
(define initial-screen (make-screen 0))

(define frames 0)
(define start-time (current-seconds))
(define (current-fps)
  (define time-since (- (current-seconds) start-time))
  (cond
    [(zero? time-since)
     tick-rate]
    [(zero? frames)
     tick-rate]
    [else
     (/ time-since frames)]))

(define step-simulation
  (match-lambda
    [(struct screen (time))
     (define time-step tick-rate)
     (set! frames (add1 frames))
     (simulate! collide! the-simulation time-step)
     (when (= (modulo frames 100) 0)
       (add-fresh-bullets!))
     (make-screen (+ time time-step))]))

(define (ship-steer s key)
  (match s
    [(struct screen (time))
     (define speed-adj (* speed 2 tick-rate))
     (define adjustment
       (match key
         ["up" (vector 0.0 speed-adj)]
         ["down" (vector 0.0 (* -1 speed-adj))]
         ["right" (vector speed-adj 0.0)]
         ["left" (vector (* -1 speed-adj) 0.0)]
         [_ (vector 0.0 0.0)]))
     (body-posn-adjust! the-ship adjustment)
     (make-screen time)]))

(define draw-screen
  (match-lambda
    [(struct screen (time))
     (for/fold ([s (place-image (text (format "FPS: ~a" (floor (/ 1 (current-fps))))
                                      14 "red")
                                0 0
                                (empty-scene (* scale screen-width) (* scale screen-height)))])
       ([b (in-list (simulation-bodies the-simulation))])
       (match b
         [(struct body (layer (vector x y) radius vel))
          (place-image (case layer
                         [(ship) ship]
                         [(bullet) bullet])
                       (* scale x)
                       (* scale (- screen-height y))
                       s)]))]))

(big-bang initial-screen
          (on-tick
           step-simulation
           tick-rate)
          (on-key
           ship-steer)
          (on-draw
           draw-screen)
          (stop-when
           (match-lambda
             [(struct screen (time))
              dead?])))
