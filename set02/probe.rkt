;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; probe.rkt : Movement of a probe(circle) in a trap(square).

(require rackunit)
(require "extras.rkt")

(provide
 probe-at
 probe-turned-left
 probe-turned-right
 probe-forward
 probe-north?
 probe-south?
 probe-east?
 probe-west?)

;; check the location of file for automated testing
;; (check-location "02" "probe.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;; diameter of the probe (circle) in cm
(define PROBE-DIAMETER 40)

;; side of the trap (square) in cm
(define TRAP-SIDE 347)

;; moving limit of the probe inside the trap so that
;; it doesn't past the wall of trap
(define PROBE-MOVING-LIMIT
  (- (floor (/ TRAP-SIDE 2)) (/ PROBE-DIAMETER 2)))

;; directions
(define NORTH "north")
(define EAST "east")
(define SOUTH "south")
(define WEST "west")


;; A Direction is one of
;; -- NORTH
;; -- EAST
;; -- SOUTH
;; -- WEST
;; INTERPRETATION: self-evident

;; direction-fn : Direction -> ??
#|
(define (direction-fn dir)
  (cond
    [(string=? dir NORTH) ...]
    [(string=? dir EAST) ...]
    [(string=? dir SOUTH) ...]
    [(string=? dir WEST) ...]))
|#


(define-struct probe (x y direction))

;; A Probe is a 
;; (make-probe Integer Integer Direction)
;; INTERPRETATION:
;; x is the x-coordinate of the probe
;; y is the y-coordinate of the probe
;; direction is the direction towards which
;; the probe will move (probe is facing)


;; probe-fn : Probe -> ??
#|
(define (probe-fn p)
  (...
    (probe-x p)
    (probe-y p)
    (probe-direction p)))
|#

(define probe-at-origin-facing-north (make-probe 0 0 NORTH))
(define probe-at-origin-facing-east (make-probe 0 0 EAST))
(define probe-at-origin-facing-south (make-probe 0 0 SOUTH))
(define probe-at-origin-facing-west (make-probe 0 0 WEST))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-at : Integer Integer -> Probe
;; GIVEN: an x-coordinate and a y-coordinate
;; WHERE: these coordinates leave the robot entirely inside the trap
;; RETURNS: a probe with its center at those coordinates, facing north.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (probe-at 0 0) (make-probe 0 0 NORTH))
  (check-equal? (probe-at 15 -20) (make-probe 15 -20 NORTH)))

;; DESIGN STRATEGY: Combine simpler functions

(define (probe-at x y)
  (make-probe x y NORTH))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-turned-left : Probe -> Probe
;; probe-turned-right : Probe -> Probe
;; GIVEN: a probe
;; RETURNS: a probe like the original, but turned 90
;; degrees either left or right.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (probe-turned-left probe-at-origin-facing-north) probe-at-origin-facing-west)
  (check-equal? (probe-turned-left probe-at-origin-facing-east) probe-at-origin-facing-north)
  (check-equal? (probe-turned-left probe-at-origin-facing-south) probe-at-origin-facing-east)
  (check-equal? (probe-turned-left probe-at-origin-facing-west) probe-at-origin-facing-south))

(begin-for-test
  (check-equal? (probe-turned-right probe-at-origin-facing-north) probe-at-origin-facing-east)
  (check-equal? (probe-turned-right probe-at-origin-facing-east) probe-at-origin-facing-south)
  (check-equal? (probe-turned-right probe-at-origin-facing-south) probe-at-origin-facing-west)
  (check-equal? (probe-turned-right probe-at-origin-facing-west) probe-at-origin-facing-north))

;; DESIGN STRATEGY: Use template for Probe on p

(define (probe-turned-left p)
  (make-probe
   (probe-x p)
   (probe-y p)
   (direction-after-left-turn (probe-direction p))))

(define (probe-turned-right p)
  (make-probe
   (probe-x p)
   (probe-y p)
   (direction-after-right-turn (probe-direction p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; direction-after-left-turn : Direction -> Direction
;; direction-after-right-turn : Direction -> Direction
;; GIVEN: a direction dir
;; RETURNS: a direction after turning 90 degrees either left or right.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (direction-after-left-turn NORTH) WEST)
  (check-equal? (direction-after-left-turn EAST) NORTH)
  (check-equal? (direction-after-left-turn SOUTH) EAST)
  (check-equal? (direction-after-left-turn WEST) SOUTH))

(begin-for-test
  (check-equal? (direction-after-right-turn NORTH) EAST)
  (check-equal? (direction-after-right-turn EAST) SOUTH)
  (check-equal? (direction-after-right-turn SOUTH) WEST)
  (check-equal? (direction-after-right-turn WEST) NORTH))

;; DESIGN STRATEGY: Use template for Direction on dir

(define (direction-after-left-turn dir)
  (cond
    [(string=? dir NORTH) WEST]
    [(string=? dir EAST) NORTH]
    [(string=? dir SOUTH) EAST]
    [(string=? dir WEST) SOUTH]))

(define (direction-after-right-turn dir)
  (cond
    [(string=? dir NORTH) EAST]
    [(string=? dir EAST) SOUTH]
    [(string=? dir SOUTH) WEST]
    [(string=? dir WEST) NORTH]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-forward : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward by the
;; specified distance.  If moving forward the specified distance would
;; cause the probe to hit any wall of the trap, then the probe should 
;; move as far as it can inside the trap, and then stop.


;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (probe-forward probe-at-origin-facing-north 153) (make-probe 0 -153 NORTH))
  (check-equal? (probe-forward probe-at-origin-facing-north 160) (make-probe 0 -153 NORTH))
  (check-equal? (probe-forward probe-at-origin-facing-north 140) (make-probe 0 -140 NORTH))
  
  (check-equal? (probe-forward probe-at-origin-facing-east 153) (make-probe 153 0 EAST))
  (check-equal? (probe-forward probe-at-origin-facing-east 160) (make-probe 153 0 EAST))
  (check-equal? (probe-forward probe-at-origin-facing-east 140) (make-probe 140 0 EAST))
  
  (check-equal? (probe-forward probe-at-origin-facing-south 153) (make-probe 0 153 SOUTH))
  (check-equal? (probe-forward probe-at-origin-facing-south 160) (make-probe 0 153 SOUTH))
  (check-equal? (probe-forward probe-at-origin-facing-south 140) (make-probe 0 140 SOUTH))
  
  (check-equal? (probe-forward probe-at-origin-facing-west 153) (make-probe -153 0 WEST))
  (check-equal? (probe-forward probe-at-origin-facing-west 160) (make-probe -153 0 WEST))
  (check-equal? (probe-forward probe-at-origin-facing-west 140) (make-probe -140 0 WEST)))


;; DESIGN STRATEGY: Use template for Probe on p and Direction on (probe-direction p)

(define (probe-forward p dist)
  (cond
    [(string=? (probe-direction p) NORTH) (probe-forward-north p dist)]
    [(string=? (probe-direction p) EAST) (probe-forward-east p dist)]
    [(string=? (probe-direction p) SOUTH) (probe-forward-south p dist)]
    [(string=? (probe-direction p) WEST) (probe-forward-west p dist)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-forward-north : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward by the specified
;; distance towards north direction. If moving forward the specified distance
;; would cause the probe to hit any wall of the trap, then the probe should 
;; move as far as it can inside the trap, and then stop.
;; WHERE: given probe must be facing towards north


;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (probe-forward-north probe-at-origin-facing-north 153) (make-probe 0 -153 NORTH))
  (check-equal? (probe-forward-north probe-at-origin-facing-north 160) (make-probe 0 -153 NORTH))
  (check-equal? (probe-forward-north probe-at-origin-facing-north 140) (make-probe 0 -140 NORTH)))


;; DESIGN STRATEGY: Use template for Probe on p

(define (probe-forward-north p dist)
  (make-probe
   (probe-x p)
   (max (- (probe-y p) dist) (- 0 PROBE-MOVING-LIMIT))
   (probe-direction p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-forward-south : Probe PosInt -> Probe
;; GIVEN: a probe and a distance 
;; RETURNS: a probe like the given one, but moved forward by the
;; specified distance towards south direction. If moving forward the specified
;; distance would cause the probe to hit any wall of the trap, then the probe
;; should move as far as it can inside the trap, and then stop.
;; WHERE: given probe must be facing towards south

;; EXAMPLES\TESTS:

(begin-for-test  
  (check-equal? (probe-forward-south probe-at-origin-facing-south 153) (make-probe 0 153 SOUTH))
  (check-equal? (probe-forward-south probe-at-origin-facing-south 160) (make-probe 0 153 SOUTH))
  (check-equal? (probe-forward-south probe-at-origin-facing-south 140) (make-probe 0 140 SOUTH)))

;; DESIGN STRATEGY: Use template for Probe on p

(define (probe-forward-south p dist)
  (make-probe
   (probe-x p)
   (min (+ (probe-y p) dist) PROBE-MOVING-LIMIT)
   (probe-direction p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-forward-west : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward by the specified
;; distance towards west direction. If moving forward the specified distance
;; would cause the probe to hit any wall of the trap, then the probe should
;; move as far as it can inside the trap, and then stop.
;; WHERE: given probe must be facing towards west

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (probe-forward-west probe-at-origin-facing-west 153) (make-probe -153 0 WEST))
  (check-equal? (probe-forward-west probe-at-origin-facing-west 160) (make-probe -153 0 WEST))
  (check-equal? (probe-forward-west probe-at-origin-facing-west 140) (make-probe -140 0 WEST)))

;; DESIGN STRATEGY: Use template for Probe on p

(define (probe-forward-west p dist)
  (make-probe
   (max (- (probe-x p) dist) (- 0 PROBE-MOVING-LIMIT))
   (probe-y p)
   (probe-direction p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-forward-east : Probe PosInt -> Probe
;; GIVEN: a probe and a distance
;; RETURNS: a probe like the given one, but moved forward by the
;; specified distance towards east direction.  If moving forward
;; the specified distance would cause the probe to hit any wall
;; of the trap, then the probe should move as far as it can inside
;; the trap, and then stop.
;; WHERE: given probe must be facing towards east

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (probe-forward-east probe-at-origin-facing-east 153) (make-probe 153 0 EAST))
  (check-equal? (probe-forward-east probe-at-origin-facing-east 160) (make-probe 153 0 EAST))
  (check-equal? (probe-forward-east probe-at-origin-facing-east 140) (make-probe 140 0 EAST)))

;; DESIGN STRATEGY: Use template for Probe on p

(define (probe-forward-east p dist)
  (make-probe
   (min (+ (probe-x p) dist) PROBE-MOVING-LIMIT)
   (probe-y p)
   (probe-direction p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; probe-north? : Probe -> Boolean
;; probe-south? : Probe -> Boolean
;; probe-east? : Probe -> Boolean
;; probe-west? : Probe -> Boolean
;; GIVEN: a probe
;; ANSWERS: whether the probe is facing in the specified direction.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (probe-north? probe-at-origin-facing-north) true)
  (check-equal? (probe-north? probe-at-origin-facing-south) false))

(begin-for-test
  (check-equal? (probe-south? probe-at-origin-facing-south) true)
  (check-equal? (probe-south? probe-at-origin-facing-north) false))

(begin-for-test
  (check-equal? (probe-east? probe-at-origin-facing-east) true)
  (check-equal? (probe-east? probe-at-origin-facing-west) false))

(begin-for-test
  (check-equal? (probe-west? probe-at-origin-facing-west) true)
  (check-equal? (probe-west? probe-at-origin-facing-south) false))

;; DESIGN STRATEGY: Cases on Probe

(define (probe-north? p)
  (string=? (probe-direction p) NORTH))

(define (probe-south? p)
  (string=? (probe-direction p) SOUTH))

(define (probe-east? p)
  (string=? (probe-direction p) EAST))

(define (probe-west? p)
  (string=? (probe-direction p) WEST))
