;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-1.rkt: moving ranctangles.  
;; There are two ractangles, each moves from the initial position as per its
;; velocities in x and y directions and it bounces smoothly off the edge of
;; the canvas. The user can pause/unpause the entire scene with the space bar.

;; start with (screensaver 0.5)

(require rackunit)
(require "extras.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 screensaver 
 initial-world
 world-after-tick
 world-after-key-event
 world-rect1
 world-rect2
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy)

;; check the location of file for automated testing
;; (check-location "03" "screensaver-1.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCREENSAVER FUNCTION:

;; screensaver: PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world

;; EXAMPLES: (screensaver 0.5) will run the simulation starting with the
;; initial state at 0.5 seconds/tick speed

;; DESIGN STRATEGY: combine simpler functions
(define (screensaver speed)
  (big-bang (initial-world ANY-VALUE)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS

;; dimensions of the rectangle
(define RECTANGLE-WIDTH 60)
(define RECTANGLE-HEIGHT 50)
(define HALF-RECTANGLE-WIDTH  (/ RECTANGLE-WIDTH 2))
(define HALF-RECTANGLE-HEIGHT (/ RECTANGLE-HEIGHT 2))

;; font size and color for the text displayed in the the center of the rectangle
(define FONT-SIZE 11)
(define COLOR-BLUE "blue")
(define OUTLINE "outline")

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

;; image of the empty canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; minimum and maximum limit of x and y co-ordinate's of the center of rectangle
(define X-MIN-LIMIT HALF-RECTANGLE-WIDTH)
(define X-MAX-LIMIT (- CANVAS-WIDTH HALF-RECTANGLE-WIDTH))
(define Y-MIN-LIMIT HALF-RECTANGLE-HEIGHT)
(define Y-MAX-LIMIT (- CANVAS-HEIGHT HALF-RECTANGLE-HEIGHT))

;; any value for giving as a input which is going to be ignored by a function
(define ANY-VALUE 0)

;; initial x and y coordinates and velocities of two rectangles and paused flag
(define INIT-RECT-1-X 200)
(define INIT-RECT-1-Y 100)
(define INIT-RECT-1-VX -12)
(define INIT-RECT-1-VY 20)
(define INIT-RECT-2-X 200)
(define INIT-RECT-2-Y 200)
(define INIT-RECT-2-VX 23)
(define INIT-RECT-2-VY -14)
(define INIT-PAUSED-TRUE true)

;; images of rectangles
;; an outline blue rectangle, RECTANGLE-WIDTH pixels wide and RECTANGLE-HEIGHT
;; pixels high and its current velocity is displayed as a string in the center with
;; font size = FONT-SIZE and font color = COLOR-BLUE.
(define IMAGE-OF-RECT-AT_200_100-VEL_-12_20
  (overlay
   (text "(-12, 20)" FONT-SIZE COLOR-BLUE)
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT OUTLINE COLOR-BLUE)))

(define IMAGE-OF-RECT-AT_200_200-VEL_23_-14
  (overlay
   (text "(23, -14)" FONT-SIZE COLOR-BLUE)
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT OUTLINE COLOR-BLUE)))

;; image of the paused world
;; it is a canvas with two rectangle images, centered at (200,100) and (200,200).
(define IMAGE-OF-PAUSED-WORLD
  (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20 200 100
    (place-image IMAGE-OF-RECT-AT_200_200-VEL_23_-14 200 200
      EMPTY-CANVAS)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS:

(define-struct rect (x y vx vy))
;; A Rectangle is a (make-rect NonNegInt NonNegInt Integer Integer)
;; INTERPRETATION: 
;; x and y are the co-ordinates of the center point of the rectangle, 
;; vx and vy are the velocities of the rectangle in x and y direction
;; respectively in pixels/tick.

;; TEMPLATE:
;; rect-fn : Rectangle -> ??
#|
(define (rect-fn r)
  (...
   (rect-x r)
   (rect-y r)
   (rect-vx r)
   (rect-vy r)))
|#

;; examples of rectangles, for testing

(define RECT-AT_200_100-VEL_-12_20 (make-rect 200 100 -12 20))
(define RECT-AT_188_120-VEL_-12_20 (make-rect 188 120 -12 20))
(define RECT-AT_200_200-VEL_23_-14 (make-rect 200 200 23 -14))
(define RECT-AT_223_186-VEL_23_-14 (make-rect 223 186 23 -14))
(define RECT-AT_40_30-VEL_-12_-20 (make-rect 40 30 -12 -20))
(define RECT-AT_30_25-VEL_12_20 (make-rect 30 25 12 20))
(define RECT-AT_365_30-VEL_12_-20 (make-rect 365 30 12 -20))
(define RECT-AT_370_25-VEL_-12_20 (make-rect 370 25 -12 20))
(define RECT-AT_365_265-VEL_12_20 (make-rect 365 265 12 20))
(define RECT-AT_370_275-VEL_-12_-20 (make-rect 370 275 -12 -20))
(define RECT-AT_40_270-VEL_-12_20 (make-rect 40 270 -12 20))
(define RECT-AT_30_275-VEL_12_-20 (make-rect 30 275 12 -20))
(define RECT-AT_40_100-VEL_-12_20 (make-rect 40 100 -12 20))
(define RECT-AT_30_117-VEL_12_20 (make-rect 30 117 12 20))
(define RECT-AT_100_35-VEL_-12_-20 (make-rect 100 35 -12 -20))
(define RECT-AT_94_25-VEL_-12_20 (make-rect 94 25 -12 20))
(define RECT-AT_360_100-VEL_12_20 (make-rect 360 100 12 20))
(define RECT-AT_370_117-VEL_-12_20 (make-rect 370 117 -12 20))
(define RECT-AT_100_260-VEL_12_20 (make-rect 100 260 12 20))
(define RECT-AT_109_275-VEL_12_-20 (make-rect 109 275 12 -20))


(define-struct world (rect1 rect2 paused?))
;; A WorldState is a (make-world Rectangle Rectangle Boolean)
;; INTERPRETATION:
;; rect1 and rect2 are the two rectangles
;; paused? describes whether or not the world is paused

;; TEMPLATE:
;; world-fn : World -> ??

#|
(define (world-fn w)
  (...
   (world-rect1 w)
   (world-rect2 w)
   (world-paused? w)))
|#

;; examples of worlds, for testing

(define PAUSED-WORLD
  (make-world
    RECT-AT_200_100-VEL_-12_20
    RECT-AT_200_200-VEL_23_-14
    true))

(define UNPAUSED-WORLD
  (make-world
    RECT-AT_200_100-VEL_-12_20
    RECT-AT_200_200-VEL_23_-14
    false))

(define UNPAUSED-WORLD-AFTER-TICK
  (make-world
    RECT-AT_188_120-VEL_-12_20
    RECT-AT_223_186-VEL_23_-14
    false))

;; examples of KeyEvents, for testing
(define PAUSE-KEY-EVENT " ")
(define NON-PAUSE-KEY-EVENT "q")   

;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world: Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLES:
;; (initial-world 0) = PAUSED-WORLD

;; DESIGN STRATEGY: combine simpler functions
(define (initial-world n)
  (make-world
   (new-rectangle INIT-RECT-1-X INIT-RECT-1-Y INIT-RECT-1-VX INIT-RECT-1-VY)
   (new-rectangle INIT-RECT-2-X INIT-RECT-2-Y INIT-RECT-2-VX INIT-RECT-2-VY)
   INIT-PAUSED-TRUE))

;; TESTS:
(begin-for-test
  (check-equal?
   (initial-world ANY-VALUE)
   PAUSED-WORLD
   "Initial world with given parameters should be created."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-tick: WorldState -> WorldState
;; GIVEN: the world state w
;; RETURNS: the world state that should follow the given world state
;; after a tick.

;; EXAMPLES:
;; (world-after-tick PAUSED-WORLD) = PAUSED-WORLD
;; (world-after-tick UNPAUSED-WORLD) = UNPAUSED-WORLD-AFTER-TICK

;; DESIGN STRATEGY: Use template for WorldState on w

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rect-after-tick (world-rect1 w))
       (rect-after-tick (world-rect2 w))
       (world-paused? w))))

;; TESTS:

(begin-for-test
  (check-equal?
   (world-after-tick PAUSED-WORLD)
   PAUSED-WORLD
   "Rectangles of paused world should not move")

  (check-equal?
   (world-after-tick UNPAUSED-WORLD)
   UNPAUSED-WORLD-AFTER-TICK
   "Rectangles of unpaused world should move according to their x & y velocities."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick: Rectangle -> Rectangle
;; GIVEN: the rectangle r
;; RETURNS: the rectangle that should move as per x and y velocities of given
;; rectangle after a tick.

;; EXAMPLES:
;; (rect-after-tick RECT-AT_200_100-VEL_-12_20) = RECT-AT_188_120-VEL_-12_20
;; (rect-after-tick RECT-AT_40_30-VEL_-12_-20) = RECT-AT_30_25-VEL_12_20
;; (rect-after-tick RECT-AT_365_30-VEL_12_-20) = RECT-AT_370_25-VEL_-12_20
;; (rect-after-tick RECT-AT_365_265-VEL_12_20) = RECT-AT_370_275-VEL_-12_-20
;; (rect-after-tick RECT-AT_40_270-VEL_-12_20) = RECT-AT_30_275-VEL_12_-20
;; (rect-after-tick RECT-AT_40_100-VEL_-12_20) = RECT-AT_30_117-VEL_12_20
;; (rect-after-tick RECT-AT_100_35-VEL_-12_-20) = RECT-AT_94_25-VEL_-12_20
;; (rect-after-tick RECT-AT_360_100-VEL_12_20) = RECT-AT_370_117-VEL_-12_20
;; (rect-after-tick RECT-AT_100_260-VEL_12_20) = RECT-AT_109_275-VEL_12_-20

;; DESIGN STRATEGY: Cases on whether Rectangle touch or go past corner/side
;; of canvas on the next tick
(define (rect-after-tick r)
  (cond
    [(rect-touch-or-go-past-left-top-corner? r)
     (rect-after-tick-at-left-top-corner r)]
    
    [(rect-touch-or-go-past-right-top-corner? r)
     (rect-after-tick-at-right-top-corner r)]
    
    [(rect-touch-or-go-past-right-bottom-corner? r)
     (rect-after-tick-at-right-bottom-corner r)]
    
    [(rect-touch-or-go-past-left-bottom-corner? r)
     (rect-after-tick-at-left-bottom-corner r)]
    
    [(rect-touch-or-go-past-left-side? r)
     (rect-after-tick-at-left-side r)]
    
    [(rect-touch-or-go-past-right-side? r)
     (rect-after-tick-at-right-side r)]
    
    [(rect-touch-or-go-past-top-side? r)
     (rect-after-tick-at-top-side r)]
    
    [(rect-touch-or-go-past-bottom-side? r)
     (rect-after-tick-at-bottom-side r)]
    
    [else (rect-after-tick-inside-canvas r)]))

;; TESTS:
(begin-for-test

  ;; rectangle inside the canvas
  (check-equal?
   (rect-after-tick RECT-AT_200_100-VEL_-12_20)
   RECT-AT_188_120-VEL_-12_20
   "RECT-AT_200_100-VEL_-12_20 should move to RECT-AT_188_120-VEL_-12_20 after the tick.")

  ;; rectangle go past any corner
  (check-equal?
   (rect-after-tick RECT-AT_40_30-VEL_-12_-20)
   RECT-AT_30_25-VEL_12_20
   "RECT-AT_40_30-VEL_-12_-20 should move to RECT-AT_30_25-VEL_12_20 after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_365_30-VEL_12_-20)
   RECT-AT_370_25-VEL_-12_20
   "RECT-AT_365_30-VEL_12_-20 should move to RECT-AT_370_25-VEL_-12_20 after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_365_265-VEL_12_20)
   RECT-AT_370_275-VEL_-12_-20
   "RECT-AT_365_265-VEL_12_20 should move to RECT-AT_370_275-VEL_-12_-20 after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_40_270-VEL_-12_20)
   RECT-AT_30_275-VEL_12_-20
   "RECT-AT_40_270-VEL_-12_20 should move to RECT-AT_30_275-VEL_12_-20 after the tick.")

  ;; rectangle go past any side
  (check-equal?
   (rect-after-tick RECT-AT_40_100-VEL_-12_20)
   RECT-AT_30_117-VEL_12_20
   "RECT-AT_40_100-VEL_-12_20 should move to RECT-AT_30_117-VEL_12_20 after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_100_35-VEL_-12_-20)
   RECT-AT_94_25-VEL_-12_20
   "RECT-AT_100_35-VEL_-12_-20 should move to RECT-AT_94_25-VEL_-12_20 after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_360_100-VEL_12_20)
   RECT-AT_370_117-VEL_-12_20
   "RECT-AT_360_100-VEL_12_20 should move to RECT-AT_370_117-VEL_-12_20 after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_100_260-VEL_12_20)
   RECT-AT_109_275-VEL_12_-20
   "RECT-AT_100_260-VEL_12_20 should move to RECT-AT_109_275-VEL_12_-20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-touch-or-go-past-left-top-corner?: Rectangle -> Boolean
;; rect-touch-or-go-past-right-top-corner?: Rectangle -> Boolean
;; rect-touch-or-go-past-right-bottom-corner?: Rectangle -> Boolean
;; rect-touch-or-go-past-left-bottom-corner?: Rectangle -> Boolean
;; GIVEN: the rectangle r
;; ANSWERS: wheather or not the rectangle is touching or going past the
;; specific corner after a tick.

;; EXAMPLES:
;; (rect-touch-or-go-past-left-top-corner? RECT-AT_40_30-VEL_-12_-20) = true
;; (rect-touch-or-go-past-left-top-corner? RECT-AT_40_100-VEL_-12_20) = false
;; (rect-touch-or-go-past-right-top-corner? RECT-AT_365_30-VEL_12_-20) = true
;; (rect-touch-or-go-past-right-top-corner? RECT-AT_100_35-VEL_-12_-20) = false
;; (rect-touch-or-go-past-right-bottom-corner? RECT-AT_365_265-VEL_12_20) = true
;; (rect-touch-or-go-past-right-bottom-corner? RECT-AT_360_100-VEL_12_20) = false
;; (rect-touch-or-go-past-left-bottom-corner? RECT-AT_40_270-VEL_-12_20) = true
;; (rect-touch-or-go-past-left-bottom-corner? RECT-AT_100_260-VEL_12_20) = false

;; DESIGN STRATEGY: combine simpler functions
(define (rect-touch-or-go-past-left-top-corner? r)
  (and
   (rect-touch-or-go-past-left-side? r)
   (rect-touch-or-go-past-top-side? r)))

(define (rect-touch-or-go-past-right-top-corner? r)
  (and
   (rect-touch-or-go-past-right-side? r)
   (rect-touch-or-go-past-top-side? r)))

(define (rect-touch-or-go-past-right-bottom-corner? r)
  (and
   (rect-touch-or-go-past-right-side? r)
   (rect-touch-or-go-past-bottom-side? r)))

(define (rect-touch-or-go-past-left-bottom-corner? r)
  (and
   (rect-touch-or-go-past-left-side? r)
   (rect-touch-or-go-past-bottom-side? r)))

;;TESTS:
(begin-for-test
  (check-equal?
   (rect-touch-or-go-past-left-top-corner? RECT-AT_40_30-VEL_-12_-20)
   true
   "RECT-AT_40_30-VEL_-12_-20 should touch or go past left top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-left-top-corner? RECT-AT_40_100-VEL_-12_20)
   false
   "RECT-AT_40_100-VEL_-12_20 should not touch or go past left top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-top-corner? RECT-AT_365_30-VEL_12_-20)
   true
   "RECT-AT_365_30-VEL_12_-20 should touch or go past right top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-top-corner? RECT-AT_100_35-VEL_-12_-20)
   false
   "RECT-AT_100_35-VEL_-12_-20 should not touch or go past right top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-bottom-corner? RECT-AT_365_265-VEL_12_20)
   true
   "RECT-AT_365_265-VEL_12_20 should touch or go past right bottom corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-bottom-corner? RECT-AT_360_100-VEL_12_20)
   false
   "RECT-AT_360_100-VEL_12_20 should not touch or go past right bottom corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-left-bottom-corner? RECT-AT_40_270-VEL_-12_20)
   true
   "RECT-AT_40_270-VEL_-12_20 should touch or go past left bottom corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-left-bottom-corner? RECT-AT_100_260-VEL_12_20)
   false
   "RECT-AT_100_260-VEL_12_20 should not touch or go past left bottom corner at the next tick."))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-touch-or-go-past-left-side?: Rectangle -> Boolean
;; rect-touch-or-go-past-top-side?: Rectangle -> Boolean
;; rect-touch-or-go-past-right-side?: Rectangle -> Boolean
;; rect-touch-or-go-past-bottom-side?: Rectangle -> Boolean
;; GIVEN: the rectangle r
;; ANSWERS: wheather or not the rectangle is touching or going past the
;; specific side after a tick.

;; EXAMPLES:
;; (rect-touch-or-go-past-left-side? RECT-AT_40_100-VEL_-12_20) = true
;; (rect-touch-or-go-past-left-side? RECT-AT_200_100-VEL_-12_20) = false
;; (rect-touch-or-go-past-top-side? RECT-AT_100_35-VEL_-12_-20) = true
;; (rect-touch-or-go-past-top-side? RECT-AT_200_100-VEL_-12_20) = false
;; (rect-touch-or-go-past-right-side? RECT-AT_360_100-VEL_12_20) = true
;; (rect-touch-or-go-past-right-side? RECT-AT_200_100-VEL_-12_20) = false
;; (rect-touch-or-go-past-bottom-side? RECT-AT_100_260-VEL_12_20) = true
;; (rect-touch-or-go-past-bottom-side? RECT-AT_200_100-VEL_-12_20) = false

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-touch-or-go-past-left-side? r)
  (<= (+ (rect-x r) (rect-vx r)) X-MIN-LIMIT))

(define (rect-touch-or-go-past-top-side? r)
  (<= (+ (rect-y r) (rect-vy r)) Y-MIN-LIMIT))
  
(define (rect-touch-or-go-past-right-side? r)
  (>= (+ (rect-x r) (rect-vx r)) X-MAX-LIMIT))

(define (rect-touch-or-go-past-bottom-side? r)
  (>= (+ (rect-y r) (rect-vy r)) Y-MAX-LIMIT))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-touch-or-go-past-left-side? RECT-AT_40_100-VEL_-12_20)
   true
   "RECT-AT_40_100-VEL_-12_20 should touch or go past left side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-left-side? RECT-AT_200_100-VEL_-12_20)
   false
   "RECT-AT_200_100-VEL_-12_20 should not touch or go past left side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-top-side? RECT-AT_100_35-VEL_-12_-20)
   true
   "RECT-AT_100_35-VEL_-12_-20 should touch or go past top side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-top-side? RECT-AT_200_100-VEL_-12_20)
   false
   "RECT-AT_200_100-VEL_-12_20 should not touch or go past top side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-right-side? RECT-AT_360_100-VEL_12_20)
   true
   "RECT-AT_360_100-VEL_12_20 should touch or go past right side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-right-side? RECT-AT_200_100-VEL_-12_20)
   false
   "RECT-AT_200_100-VEL_-12_20 should not touch or go past right side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-bottom-side? RECT-AT_100_260-VEL_12_20)
   true
   "RECT-AT_100_260-VEL_12_20 should touch or go past bottom side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-bottom-side? RECT-AT_200_100-VEL_-12_20)
   false
   "RECT-AT_200_100-VEL_-12_20 should not touch or go past bottom side at the next tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-left-top-corner: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past left top corner
;; of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its left and top sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-left-top-corner RECT-AT_40_30-VEL_-12_-20) = RECT-AT_30_25-VEL_12_20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-left-top-corner r)
  (new-rectangle 
   X-MIN-LIMIT
   Y-MIN-LIMIT
   (- (rect-vx r))
   (- (rect-vy r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-left-top-corner RECT-AT_40_30-VEL_-12_-20)
   RECT-AT_30_25-VEL_12_20
   "RECT-AT_40_30-VEL_-12_-20 should move to RECT-AT_30_25-VEL_12_20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-right-top-corner: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past right top corner
;; of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its right and top sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-right-top-corner RECT-AT_365_30-VEL_12_-20) = RECT-AT_370_25-VEL_-12_20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-right-top-corner r)
  (new-rectangle
   X-MAX-LIMIT
   Y-MIN-LIMIT
   (- (rect-vx r))
   (- (rect-vy r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-right-top-corner RECT-AT_365_30-VEL_12_-20)
   RECT-AT_370_25-VEL_-12_20
   "RECT-AT_365_30-VEL_12_-20 should move to RECT-AT_370_25-VEL_-12_20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-right-bottom-corner: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past right bottom corner
;; of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its right and bottom sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-right-bottom-corner RECT-AT_365_265-VEL_12_20) = RECT-AT_370_275-VEL_-12_-20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-right-bottom-corner r)
  (new-rectangle
   X-MAX-LIMIT
   Y-MAX-LIMIT
   (- (rect-vx r))
   (- (rect-vy r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-right-bottom-corner RECT-AT_365_265-VEL_12_20)
   RECT-AT_370_275-VEL_-12_-20
   "RECT-AT_365_265-VEL_12_20 should move to RECT-AT_370_275-VEL_-12_-20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-left-bottom-corner: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past left bottom corner
;; of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its left and bottom sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-left-bottom-corner RECT-AT_40_270-VEL_-12_20) = RECT-AT_30_275-VEL_12_-20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-left-bottom-corner r)
  (new-rectangle
   X-MIN-LIMIT
   Y-MAX-LIMIT
   (- (rect-vx r))
   (- (rect-vy r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-left-bottom-corner RECT-AT_40_270-VEL_-12_20)
   RECT-AT_30_275-VEL_12_-20
   "RECT-AT_40_270-VEL_-12_20 should move to RECT-AT_30_275-VEL_12_-20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-left-side: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past left side of the
;; canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its left side touches the canvas and its center
;; remains on the moving line with only x velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-left-side RECT-AT_40_100-VEL_-12_20) = RECT-AT_30_117-VEL_12_20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-left-side r)
  (new-rectangle
   X-MIN-LIMIT
   (rect-y-after-tick r X-MIN-LIMIT)
   (- (rect-vx r))
   (rect-vy r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-left-side RECT-AT_40_100-VEL_-12_20)
   RECT-AT_30_117-VEL_12_20
   "RECT-AT_40_100-VEL_-12_20 should move to RECT-AT_30_117-VEL_12_20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-top-side: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past top side of the
;; canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its top side touches the canvas and its center
;; remains on the moving line with only y velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-top-side RECT-AT_100_35-VEL_-12_-20) = RECT-AT_94_25-VEL_-12_20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-top-side r)
  (new-rectangle
   (rect-x-after-tick r Y-MIN-LIMIT)
   Y-MIN-LIMIT
   (rect-vx r)
   (- (rect-vy r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-top-side RECT-AT_100_35-VEL_-12_-20)
   RECT-AT_94_25-VEL_-12_20
   "RECT-AT_100_35-VEL_-12_-20 should move to RECT-AT_94_25-VEL_-12_20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-right-side: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past right side of the
;; canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its right side touches the canvas and its center
;; remains on the moving line with only x velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-right-side RECT-AT_360_100-VEL_12_20) = RECT-AT_370_117-VEL_-12_20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-right-side r)
  (new-rectangle
   X-MAX-LIMIT
   (rect-y-after-tick r X-MAX-LIMIT)
   (- (rect-vx r))
   (rect-vy r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-right-side RECT-AT_360_100-VEL_12_20)
   RECT-AT_370_117-VEL_-12_20
   "RECT-AT_360_100-VEL_12_20 should move to RECT-AT_370_117-VEL_-12_20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-bottom-side: Rectangle -> Rectangle
;; GIVEN: the rectangle which will touch or go past bottom side of the
;; canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its bottom side touches the canvas and its center
;; remains on the moving line with only y velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-bottom-side RECT-AT_100_260-VEL_12_20) = RECT-AT_109_275-VEL_12_-20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-at-bottom-side r)
  (new-rectangle
   (rect-x-after-tick r Y-MAX-LIMIT)
   Y-MAX-LIMIT
   (rect-vx r)
   (- (rect-vy r))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-at-bottom-side RECT-AT_100_260-VEL_12_20)
   RECT-AT_109_275-VEL_12_-20
   "RECT-AT_100_260-VEL_12_20 should move to RECT-AT_109_275-VEL_12_-20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-inside-canvas: Rectangle -> Rectangle
;; GIVEN: the rectangle which will not touch or go past any side of the
;; canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; moved as per their x and y velocities.
;; EXAMPLES:
;; (rect-after-tick-inside-canvas RECT-AT_200_100-VEL_-12_20) = RECT-AT_188_120-VEL_-12_20

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-tick-inside-canvas r)
  (new-rectangle
   (+ (rect-x r) (rect-vx r))
   (+ (rect-y r) (rect-vy r))
   (rect-vx r)
   (rect-vy r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick-inside-canvas RECT-AT_200_100-VEL_-12_20)
   RECT-AT_188_120-VEL_-12_20
   "RECT-AT_200_100-VEL_-12_20 should move to RECT-AT_188_120-VEL_-12_20 after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-x-after-tick: Rectangle NonNegInt -> NonNegInt
;; GIVEN: the rectangle before the tick and the y co-ordinate of the center
;; of rectangle after the tick
;; RETURNS: the x co-ordinate of the center of rectangle after the tick,
;; such as the center of the rectangle after the tick moves on the same
;; line on which the given rectangle is moving.
;; Using equation of slope: x1 = x0 + (y1 - y0)/slope here slope = vy/vx

;; EXAMPLES:
;; (rect-x-after-tick RECT-AT_100_260-VEL_12_20 Y-MAX-LIMIT) = 109

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-x-after-tick r y1)
  (+ (rect-x r)
     (inexact->exact (round (/ (* (rect-vx r) (- y1 (rect-y r))) (rect-vy r))))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-x-after-tick RECT-AT_100_260-VEL_12_20 Y-MAX-LIMIT)
   109
   "X co-ordinate of the center of the RECT-AT_100_260-VEL_12_20 after tick should be 109."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-y-after-tick: Rectangle NonNegInt -> NonNegInt
;; GIVEN: the rectangle before the tick and the y co-ordinate of the center
;; of rectangle after the tick
;; RETURNS: the x co-ordinate of the center of rectangle after the tick,
;; such as the center of the rectangle after the tick moves on the same
;; line on which the given rectangle is moving.
;; Using equation of slope: y1 = y0 + (x1 - x0)*slope here slope = vy/vx

;; EXAMPLES:
;; (rect-y-after-tick RECT-AT_360_100-VEL_12_20 X-MAX-LIMIT) = 117

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-y-after-tick r x1)
  (+ (rect-y r)
     (inexact->exact (round (/ (* (rect-vy r) (- x1 (rect-x r))) (rect-vx r))))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-y-after-tick RECT-AT_360_100-VEL_12_20 X-MAX-LIMIT)
   117
   "Y co-ordinate of the center of the RECT-AT_360_100-VEL_12_20 after tick should be 117."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-to-scene : WorldState -> Scene
;; GIVEN: the world state w
;; RETURNS: a scene that portrays the given world state.
;; EXAMPLES:
;; (world-to-scene PAUSED-WORLD) = IMAGE-OF-PAUSED-WORLD
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-to-scene w)
  (place-rectangle
   (world-rect1 w)
   (place-rectangle
    (world-rect2 w)
    EMPTY-CANVAS)))


;; place-rectangle: Rectangle Scene -> Scene
;; GIVEN: a rectangle r and a scene
;; RETURNS: a scene like the given one, but with the given rectangle painted
;; on it.
;; EXAMPLES:
;; (place-rectangle RECT-AT_200_100-VEL_-12_20 EMPTY-CANVAS)
;;                  = (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20 200 100 EMPTY-CANVAS)
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (place-rectangle r scene)
  (place-image
   (rectangle-image r)
   (rect-x r) (rect-y r)
   scene))


;; rectangle-image: Rectangle -> Image
;; GIVEN: a rectangle r
;; RETURNS: an image of the rectangle
;; It is an outline blue rectangle image, RECTANGLE-WIDTH pixels wide and RECTANGLE-HEIGHT
;; pixels high and current velocity of the given rectangle is displayed as a string
;; with font size = FONT-SIZE and font color = COLOR-BLUE in the center.
;; EXAMPLES:
;; (rectangle-image RECT-AT_200_100-VEL_-12_20) = IMAGE-OF-RECT-AT_200_100-VEL_-12_20
;; DESIGN STRATEGY: Combine simpler functions 
(define (rectangle-image r)
  (overlay
   (text (text-inside-rectangle r) FONT-SIZE COLOR-BLUE)
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT OUTLINE COLOR-BLUE)))


;; text-inside-rectangle: Rectangle -> String
;; GIVEN: a rectangle r
;; RETURNS: a string which represents the current velocity of the given rectangle
;; EXAMPLE:
;; (text-inside-rectangle RECT-AT_200_100-VEL_-12_20) = "(-12, 20)"
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (text-inside-rectangle r)
  (string-append "(" (number->string (rect-vx r)) ", "
                 (number->string (rect-vy r)) ")"))


;; TESTS:
;; note: these only test whether world-to-scene calls helper functions properly.
;; it doesn't check to see whether images portrayed are the right one!
(begin-for-test
  (check-equal?
   (world-to-scene PAUSED-WORLD)
   IMAGE-OF-PAUSED-WORLD
   "WorldState should be portrayed as a scene.")
  
  (check-equal?
   (place-rectangle RECT-AT_200_100-VEL_-12_20 EMPTY-CANVAS)
   (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20 200 100 EMPTY-CANVAS)
   "Rectangle should be placed on the empty canvas with its center at 200 100.")

  (check-equal?
   (rectangle-image RECT-AT_200_100-VEL_-12_20)
   IMAGE-OF-RECT-AT_200_100-VEL_-12_20
   "RECT-AT_200_100-VEL_-12_20 should be displayed as an image.")
  
  (check-equal?
   (text-inside-rectangle RECT-AT_200_100-VEL_-12_20)
   "(-12, 20)"
   "RECT-AT_200_100-VEL_-12_20's velocities should be represented as (-12, 20)."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-key-event: WorldState KeyEvent -> WorldState
;; GIVEN: the world state w
;; RETURNS: the world state that should follow the given world state
;; after the given keyevent.
;; on space, toggle paused? -- ignore all others

;; EXAMPLES:
;; (world-after-key-event PAUSED-WORLD pause-key-event) = UNPAUSED-WORLD
;; (world-after-key-event UNPAUSED-WORLD pause-key-event) = PAUSED-WORLD
;; (world-after-key-event PAUSED-WORLD NON-PAUSE-KEY-EVENT) = PAUSED-WORLD
;; (world-after-key-event UNPAUSED-WORLD NON-PAUSE-KEY-EVENT) = UNPAUSED-WORLD

;; DESIGN STRATEGY: Cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev " ")
     (world-with-paused-toggled w)]
    [else w]))


;; world-with-paused-toggled: WorldState -> WorldState
;; GIVEN: the world state w
;; RETURNS: a world state just like the given one, but with paused? toggled

;; EXAMPLES:
;; (world-with-paused-toggled PAUSED-WORLD PAUSE-KEY-EVENT) = UNPAUSED-WORLD
;; (world-with-paused-toggled UNPAUSED-WORLD PAUSE-KEY-EVENT) = PAUSED-WORLD
;; (world-with-paused-toggled PAUSED-WORLD NON-PAUSE-KEY-EVENT) = PAUSED-WORLD
;; (world-with-paused-toggled UNPAUSED-WORLD NON-PAUSE-KEY-EVENT) = UNPAUSED-WORLD

;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-with-paused-toggled w)
  (make-world
   (world-rect1 w)
   (world-rect2 w)
   (not (world-paused? w))))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event PAUSED-WORLD PAUSE-KEY-EVENT)
   UNPAUSED-WORLD
   "after pause key, a paused world should become unpaused")
  
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD PAUSE-KEY-EVENT)
   PAUSED-WORLD
   "after pause key, an unpaused world should become paused")
  
  (check-equal?
   (world-after-key-event PAUSED-WORLD NON-PAUSE-KEY-EVENT)
   PAUSED-WORLD
   "after a non-pause key, a paused world should be unchanged")
  
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD NON-PAUSE-KEY-EVENT)
   UNPAUSED-WORLD
   "after a non-pause key, an unpaused world should be unchanged"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; new-rectangle: NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).

;; EXAMPLES:
;; (new-rectangle 200 100 -12 20) = (make-rect 200 100 -12 20)
;; (new-rectangle 200 200 23 -14) = (make-rect 200 200 23 -14)

;; DESIGN STRATEGY: combine simpler function
(define (new-rectangle x y vx vy)
  (make-rect x y vx vy))

;; TESTS:
(begin-for-test
  (check-equal?
   (new-rectangle 200 100 -12 20)
   (make-rect 200 100 -12 20)
   "New Rectangle centered at (200, 100) with vx = -12 and vy = 20 should be created.")

  (check-equal?
   (new-rectangle 200 200 23 -14)
   (make-rect 200 200 23 -14)
   "New Rectangle centered at (200, 200) with vx = 23 and vy = -14 should be created."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; (screensaver 0.5)