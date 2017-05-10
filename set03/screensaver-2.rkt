;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-2.rkt: moving and draggable ranctangles.  
;; There are two ractangles, each moves from the initial position as per its
;; velocities in x and y directions and it bounces smoothly off the edge of
;; the canvas. The user can pause/unpause the entire scene with the space bar.
;; User can drag the rectangle with the mouse irrespective of state of the scene.
;; button-down to select, drag to move, button-up to release.

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
 rect-vy
 world-after-mouse-event
 rect-after-mouse-event
 rect-selected?
 )

;; check the location of file for automated testing
;; (check-location "03" "screensaver-2.rkt")


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
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))


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
(define COLOR-RED "red")
(define OUTLINE "outline")

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

;; image of the empty canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; circle radius and image of the outline red circle
(define CIRCLE-RADIUS 5)
(define CIRCLE-IMAGE (circle CIRCLE-RADIUS OUTLINE COLOR-RED))

;; minimum and maximum limit of x and y co-ordinate's of the center of rectangle
(define X-MIN-LIMIT HALF-RECTANGLE-WIDTH)
(define X-MAX-LIMIT (- CANVAS-WIDTH HALF-RECTANGLE-WIDTH))
(define Y-MIN-LIMIT HALF-RECTANGLE-HEIGHT)
(define Y-MAX-LIMIT (- CANVAS-HEIGHT HALF-RECTANGLE-HEIGHT))

;; any value for giving as a input which is going to be ignored by a function
(define ANY-VALUE 0)

;; initial x and y coordinates and velocities of two rectangles and paused & selected flag
(define INIT-RECT-1-X 200)
(define INIT-RECT-1-Y 100)
(define INIT-RECT-1-VX -12)
(define INIT-RECT-1-VY 20)
(define INIT-RECT-2-X 200)
(define INIT-RECT-2-Y 200)
(define INIT-RECT-2-VX 23)
(define INIT-RECT-2-VY -14)
(define INIT-PAUSED-TRUE true)
(define INIT-SELECTED-FALSE false)

;; default mouse co-ordinates
(define DEFAULT-MX 0)
(define DEFAULT-MY 0)

;; images of unselected rectangles
;; an outline COLOR-BLUE rectangle, RECTANGLE-WIDTH pixels wide and RECTANGLE-HEIGHT
;; pixels high and its current velocity is displayed as a string in the center with
;; font size = FONT-SIZE and color = COLOR-BLUE.
(define IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL
  (overlay
   (text "(-12, 20)" FONT-SIZE COLOR-BLUE)
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT OUTLINE COLOR-BLUE)))

(define IMAGE-OF-RECT-AT_200_200-VEL_23_-14-UNSEL
  (overlay
   (text "(23, -14)" FONT-SIZE COLOR-BLUE)
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT OUTLINE COLOR-BLUE)))

;; image of the paused world
;; a canvas with two rectangle images, centered at (200,100) and  (200,200).
(define IMAGE-OF-PAUSED-WORLD-UNSEL-RECT
  (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100
    (place-image IMAGE-OF-RECT-AT_200_200-VEL_23_-14-UNSEL 200 200
      EMPTY-CANVAS)))

;; image of selected rectangle without circle for mouse point
;; an outline COLOR-RED rectangle, RECTANGLE-WIDTH pixels wide and RECTANGLE-HEIGHT
;; pixels high and its current velocity is displayed as a string in the center with
;; font size = FONT-SIZE and color = COLOR-RED.

(define IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL
  (overlay
   (text "(-12, 20)" FONT-SIZE COLOR-RED)
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT OUTLINE COLOR-RED)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DATA DEFINITIONS:

(define-struct rect (x y vx vy selected? mx my))
;; A Rectangle is a (make-rect NonNegInt NonNegInt Integer Integer Boolean NonNegInt NonNegInt)
;; INTERPRETATION: 
;; x and y are the co-ordinates of the center point of the rectangle, 
;; vx and vy are the velocities of the rectangle in x and y direction
;; respectively in pixels/tick.
;; selected? describes whether or not the rectangle is selected by mouse.
;; if rectangle is selected than mx and my are the current x and y
;; co-ordinates of the mouse else its DEFAULT-MX & DEFAULT-MY.

;; TEMPLATE:
;; rect-fn : Rectangle -> ??
#|
(define (rect-fn r)
  (...
   (rect-x r)
   (rect-y r)
   (rect-vx r)
   (rect-vy r)
   (rect-selected? r)
   (rect-mx r)
   (rect-my r)))
|#

;; examples of rectangles, for testing

(define RECT-AT_200_100-VEL_-12_20-UNSEL (make-rect 200 100 -12 20 false 0 0))
(define RECT-AT_188_120-VEL_-12_20-UNSEL (make-rect 188 120 -12 20 false 0 0))

(define RECT-AT_200_200-VEL_23_-14-UNSEL (make-rect 200 200 23 -14 false 0 0))
(define RECT-AT_223_186-VEL_23_-14-UNSEL (make-rect 223 186 23 -14 false 0 0))

(define RECT-AT_40_30-VEL_-12_-20-UNSEL (make-rect 40 30 -12 -20 false 0 0))
(define RECT-AT_30_25-VEL_12_20-UNSEL (make-rect 30 25 12 20 false 0 0))

(define RECT-AT_365_30-VEL_12_-20-UNSEL (make-rect 365 30 12 -20 false 0 0))
(define RECT-AT_370_25-VEL_-12_20-UNSEL (make-rect 370 25 -12 20 false 0 0))

(define RECT-AT_365_265-VEL_12_20-UNSEL (make-rect 365 265 12 20 false 0 0))
(define RECT-AT_370_275-VEL_-12_-20-UNSEL (make-rect 370 275 -12 -20 false 0 0))

(define RECT-AT_40_270-VEL_-12_20-UNSEL (make-rect 40 270 -12 20 false 0 0))
(define RECT-AT_30_275-VEL_12_-20-UNSEL (make-rect 30 275 12 -20 false 0 0))

(define RECT-AT_40_100-VEL_-12_20-UNSEL (make-rect 40 100 -12 20 false 0 0))
(define RECT-AT_30_117-VEL_12_20-UNSEL (make-rect 30 117 12 20 false 0 0))

(define RECT-AT_100_35-VEL_-12_-20-UNSEL (make-rect 100 35 -12 -20 false 0 0))
(define RECT-AT_94_25-VEL_-12_20-UNSEL (make-rect 94 25 -12 20 false 0 0))

(define RECT-AT_360_100-VEL_12_20-UNSEL (make-rect 360 100 12 20 false 0 0))
(define RECT-AT_370_117-VEL_-12_20-UNSEL (make-rect 370 117 -12 20 false 0 0))

(define RECT-AT_100_260-VEL_12_20-UNSEL (make-rect 100 260 12 20 false 0 0))
(define RECT-AT_109_275-VEL_12_-20-UNSEL (make-rect 109 275 12 -20 false 0 0))

(define RECT-AT_200_100-VEL_-12_20-SEL_205_105 (make-rect 200 100 -12 20 true 205 105))
(define RECT-AT_200_200-VEL_23_-14-SEL_205_205 (make-rect 200 200 23 -14 true 205 205))


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

(define PAUSED-WORLD-UNSEL-RECT
  (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-UNSEL
    true))

(define UNPAUSED-WORLD-UNSEL-RECT
  (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-UNSEL
    false))

(define UNPAUSED-WORLD-UNSEL-RECT-AFTER-TICK
  (make-world
    RECT-AT_188_120-VEL_-12_20-UNSEL
    RECT-AT_223_186-VEL_23_-14-UNSEL
    false))


;; examples of KeyEvents, for testing
(define PAUSE-KEY-EVENT " ")
(define NON-PAUSE-KEY-EVENT "q")


;; examples of MouseEvents, for testing
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")
(define MOVE-EVENT "move")

;; END DATA DEFINITIONS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLES:
;; (initial-world 0) = PAUSED-WORLD-UNSEL-RECT

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
   PAUSED-WORLD-UNSEL-RECT
   "Initial world with given parameters should be created."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-tick: WorldState -> WorldState
;; GIVEN: the world state w
;; RETURNS: the world state that should follow the given world state
;; after a tick.

;; EXAMPLES:
;; (world-after-tick PAUSED-WORLD-UNSEL-RECT) = PAUSED-WORLD-UNSEL-RECT
;; (world-after-tick UNPAUSED-WORLD-UNSEL-RECT) = UNPAUSED-WORLD-UNSEL-RECT-AFTER-TICK

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
   (world-after-tick PAUSED-WORLD-UNSEL-RECT)
   PAUSED-WORLD-UNSEL-RECT
   "Rectangles of paused world should not move")

  (check-equal?
   (world-after-tick UNPAUSED-WORLD-UNSEL-RECT)
   UNPAUSED-WORLD-UNSEL-RECT-AFTER-TICK
   "Rectangles of unpaused world should move according to their x & y velocities."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick: Rectangle -> Rectangle
;; GIVEN: the rectangle r
;; RETURNS: the rectangle that should move as per x and y velocities of given
;; rectangle after a tick if the given rectangle is unselected else returns the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL) = RECT-AT_188_120-VEL_-12_20-UNSEL
;; (rect-after-tick RECT-AT_200_100-VEL_-12_20-SEL_205_105) = RECT-AT_200_100-VEL_-12_20-SEL_205_105

;; DESIGN STRATEGY: Cases on whether Rectangle is selected
(define (rect-after-tick r)
  (if
   (rect-selected? r)
   r
   (rect-after-tick-unsel r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL)
   RECT-AT_188_120-VEL_-12_20-UNSEL
   "RECT-AT_200_100-VEL_-12_20-UNSEL should move to RECT-AT_188_120-VEL_-12_20-UNSEL after the tick.")

  (check-equal?
   (rect-after-tick RECT-AT_200_100-VEL_-12_20-SEL_205_105)
   RECT-AT_200_100-VEL_-12_20-SEL_205_105
   "RECT-AT_200_100-VEL_-12_20-SEL_205_105 should not move after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-unsel: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle r
;; RETURNS: the rectangle that should move as per x and y velocities of given
;; rectangle after a tick.

;; EXAMPLES:
;; (rect-after-tick-unsel RECT-AT_200_100-VEL_-12_20-UNSEL) = RECT-AT_188_120-VEL_-12_20-UNSEL
;; (rect-after-tick-unsel RECT-AT_40_30-VEL_-12_-20-UNSEL) = RECT-AT_30_25-VEL_12_20-UNSEL
;; (rect-after-tick-unsel RECT-AT_365_30-VEL_12_-20-UNSEL) = RECT-AT_370_25-VEL_-12_20-UNSEL
;; (rect-after-tick-unsel RECT-AT_365_265-VEL_12_20-UNSEL) = RECT-AT_370_275-VEL_-12_-20-UNSEL
;; (rect-after-tick-unsel RECT-AT_40_270-VEL_-12_20-UNSEL) = RECT-AT_30_275-VEL_12_-20-UNSEL
;; (rect-after-tick-unsel RECT-AT_40_100-VEL_-12_20-UNSEL) = RECT-AT_30_117-VEL_12_20-UNSEL
;; (rect-after-tick-unsel RECT-AT_100_35-VEL_-12_-20-UNSEL) = RECT-AT_94_25-VEL_-12_20-UNSEL
;; (rect-after-tick-unsel RECT-AT_360_100-VEL_12_20-UNSEL) = RECT-AT_370_117-VEL_-12_20-UNSEL
;; (rect-after-tick-unsel RECT-AT_100_260-VEL_12_20-UNSEL) = RECT-AT_109_275-VEL_12_-20-UNSEL

;; DESIGN STRATEGY: Cases on whether Rectangle touch or go past corner/side
;; of canvas on the next tick
(define (rect-after-tick-unsel r)
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
  (check-equal?
   (rect-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL)
   RECT-AT_188_120-VEL_-12_20-UNSEL
   "RECT-AT_200_100-VEL_-12_20-UNSEL should move to RECT-AT_188_120-VEL_-12_20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_40_30-VEL_-12_-20-UNSEL)
   RECT-AT_30_25-VEL_12_20-UNSEL
   "RECT-AT_40_30-VEL_-12_-20-UNSEL should move to RECT-AT_30_25-VEL_12_20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_365_30-VEL_12_-20-UNSEL)
   RECT-AT_370_25-VEL_-12_20-UNSEL
   "RECT-AT_365_30-VEL_12_-20-UNSEL should move to RECT-AT_370_25-VEL_-12_20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_365_265-VEL_12_20-UNSEL)
   RECT-AT_370_275-VEL_-12_-20-UNSEL
   "RECT-AT_365_265-VEL_12_20-UNSEL should move to RECT-AT_370_275-VEL_-12_-20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_40_270-VEL_-12_20-UNSEL)
   RECT-AT_30_275-VEL_12_-20-UNSEL
   "RECT-AT_40_270-VEL_-12_20-UNSEL should move to RECT-AT_30_275-VEL_12_-20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_40_100-VEL_-12_20-UNSEL)
   RECT-AT_30_117-VEL_12_20-UNSEL
   "RECT-AT_40_100-VEL_-12_20-UNSEL should move to RECT-AT_30_117-VEL_12_20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_100_35-VEL_-12_-20-UNSEL)
   RECT-AT_94_25-VEL_-12_20-UNSEL
   "RECT-AT_100_35-VEL_-12_-20-UNSEL should move to RECT-AT_94_25-VEL_-12_20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_360_100-VEL_12_20-UNSEL)
   RECT-AT_370_117-VEL_-12_20-UNSEL
   "RECT-AT_360_100-VEL_12_20-UNSEL should move to RECT-AT_370_117-VEL_-12_20-UNSEL after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_100_260-VEL_12_20-UNSEL)
   RECT-AT_109_275-VEL_12_-20-UNSEL
   "RECT-AT_100_260-VEL_12_20-UNSEL should move to RECT-AT_109_275-VEL_12_-20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-touch-or-go-past-left-top-corner?: Rectangle -> Boolean
;; rect-touch-or-go-past-right-top-corner?: Rectangle -> Boolean
;; rect-touch-or-go-past-right-bottom-corner?: Rectangle -> Boolean
;; rect-touch-or-go-past-left-bottom-corner?: Rectangle -> Boolean
;; GIVEN: the unselected rectangle r
;; ANSWERS: wheather the rectangle is touching or going past the
;; specific corner after a tick.

;; EXAMPLES:
;; (rect-touch-or-go-past-left-top-corner? RECT-AT_40_30-VEL_-12_-20-UNSEL) = true
;; (rect-touch-or-go-past-left-top-corner? RECT-AT_40_100-VEL_-12_20-UNSEL) = false
;; (rect-touch-or-go-past-right-top-corner? RECT-AT_365_30-VEL_12_-20-UNSEL) = true
;; (rect-touch-or-go-past-right-top-corner? RECT-AT_100_35-VEL_-12_-20-UNSEL) = false
;; (rect-touch-or-go-past-right-bottom-corner? RECT-AT_365_265-VEL_12_20-UNSEL) = true
;; (rect-touch-or-go-past-right-bottom-corner? RECT-AT_360_100-VEL_12_20-UNSEL) = false
;; (rect-touch-or-go-past-left-bottom-corner? RECT-AT_40_270-VEL_-12_20-UNSEL) = true
;; (rect-touch-or-go-past-left-bottom-corner? RECT-AT_100_260-VEL_12_20-UNSEL) = false

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

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-touch-or-go-past-left-top-corner? RECT-AT_40_30-VEL_-12_-20-UNSEL)
   true
   "RECT-AT_40_30-VEL_-12_-20-UNSEL should touch or go past left top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-left-top-corner? RECT-AT_40_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_40_100-VEL_-12_20-UNSEL should not touch or go past left top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-top-corner? RECT-AT_365_30-VEL_12_-20-UNSEL)
   true
   "RECT-AT_365_30-VEL_12_-20-UNSEL should touch or go past right top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-top-corner? RECT-AT_100_35-VEL_-12_-20-UNSEL)
   false
   "RECT-AT_100_35-VEL_-12_-20-UNSEL should not touch or go past right top corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-bottom-corner? RECT-AT_365_265-VEL_12_20-UNSEL)
   true
   "RECT-AT_365_265-VEL_12_20-UNSEL should touch or go past right bottom corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-right-bottom-corner? RECT-AT_360_100-VEL_12_20-UNSEL)
   false
   "RECT-AT_360_100-VEL_12_20-UNSEL should not touch or go past right bottom corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-left-bottom-corner? RECT-AT_40_270-VEL_-12_20-UNSEL)
   true
   "RECT-AT_40_270-VEL_-12_20-UNSEL should touch or go past left bottom corner at the next tick.")

  (check-equal?
   (rect-touch-or-go-past-left-bottom-corner? RECT-AT_100_260-VEL_12_20-UNSEL)
   false
   "RECT-AT_100_260-VEL_12_20-UNSEL should not touch or go past left bottom corner at the next tick."))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-touch-or-go-past-left-side?: Rectangle -> Boolean
;; rect-touch-or-go-past-top-side?: Rectangle -> Boolean
;; rect-touch-or-go-past-right-side?: Rectangle -> Boolean
;; rect-touch-or-go-past-bottom-side?: Rectangle -> Boolean
;; GIVEN: the unselected rectangle r
;; ANSWERS: wheather the rectangle is touching or going past the
;; specific side after a tick.

;; EXAMPLES:
;; (rect-touch-or-go-past-left-side? RECT-AT_40_100-VEL_-12_20-UNSEL) = true
;; (rect-touch-or-go-past-left-side? RECT-AT_200_100-VEL_-12_20-UNSEL) = false
;; (rect-touch-or-go-past-top-side? RECT-AT_100_35-VEL_-12_-20-UNSEL) = true
;; (rect-touch-or-go-past-top-side? RECT-AT_200_100-VEL_-12_20-UNSEL) = false
;; (rect-touch-or-go-past-right-side? RECT-AT_360_100-VEL_12_20-UNSEL) = true
;; (rect-touch-or-go-past-right-side? RECT-AT_200_100-VEL_-12_20-UNSEL) = false
;; (rect-touch-or-go-past-bottom-side? RECT-AT_100_260-VEL_12_20-UNSEL) = true
;; (rect-touch-or-go-past-bottom-side? RECT-AT_200_100-VEL_-12_20-UNSEL) = false

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
   (rect-touch-or-go-past-left-side? RECT-AT_40_100-VEL_-12_20-UNSEL)
   true
   "RECT-AT_40_100-VEL_-12_20-UNSEL should touch or go past left side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-left-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past left side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-top-side? RECT-AT_100_35-VEL_-12_-20-UNSEL)
   true
   "RECT-AT_100_35-VEL_-12_-20-UNSEL should touch or go past top side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-top-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past top side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-right-side? RECT-AT_360_100-VEL_12_20-UNSEL)
   true
   "RECT-AT_360_100-VEL_12_20-UNSEL should touch or go past right side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-right-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past right side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-bottom-side? RECT-AT_100_260-VEL_12_20-UNSEL)
   true
   "RECT-AT_100_260-VEL_12_20-UNSEL should touch or go past bottom side at the next tick.")
  
  (check-equal?
   (rect-touch-or-go-past-bottom-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past bottom side at the next tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-left-top-corner: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past left top
;; corner of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its left and top sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-left-top-corner RECT-AT_40_30-VEL_-12_-20-UNSEL) = RECT-AT_30_25-VEL_12_20-UNSEL

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
   (rect-after-tick-at-left-top-corner RECT-AT_40_30-VEL_-12_-20-UNSEL)
   RECT-AT_30_25-VEL_12_20-UNSEL
   "RECT-AT_40_30-VEL_-12_-20-UNSEL should move to RECT-AT_30_25-VEL_12_20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-right-top-corner: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past right top
;; corner of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its right and top sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-right-top-corner RECT-AT_365_30-VEL_12_-20-UNSEL) = RECT-AT_370_25-VEL_-12_20-UNSEL

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
   (rect-after-tick-at-right-top-corner RECT-AT_365_30-VEL_12_-20-UNSEL)
   RECT-AT_370_25-VEL_-12_20-UNSEL
   "RECT-AT_365_30-VEL_12_-20-UNSEL should move to RECT-AT_370_25-VEL_-12_20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-right-bottom-corner: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past right bottom
;; corner of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its right and bottom sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-right-bottom-corner RECT-AT_365_265-VEL_12_20-UNSEL) = RECT-AT_370_275-VEL_-12_-20-UNSEL

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
   (rect-after-tick-at-right-bottom-corner RECT-AT_365_265-VEL_12_20-UNSEL)
   RECT-AT_370_275-VEL_-12_-20-UNSEL
   "RECT-AT_365_265-VEL_12_20-UNSEL should move to RECT-AT_370_275-VEL_-12_-20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-left-bottom-corner: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past left bottom
;; corner of the canvas at the next tick
;; RETURNS: the rectangle after a tick whose x and y co-ordinate are
;; updated such as its left and bottom sides touches the canvas and both
;; the velocities are reversed from the given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-left-bottom-corner RECT-AT_40_270-VEL_-12_20-UNSEL) = RECT-AT_30_275-VEL_12_-20-UNSEL

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
   (rect-after-tick-at-left-bottom-corner RECT-AT_40_270-VEL_-12_20-UNSEL)
   RECT-AT_30_275-VEL_12_-20-UNSEL
   "RECT-AT_40_270-VEL_-12_20-UNSEL should move to RECT-AT_30_275-VEL_12_-20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-left-side: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past left side
;; of the canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its left side touches the canvas and its center
;; remains on the moving line with only x velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-left-side RECT-AT_40_100-VEL_-12_20-UNSEL) = RECT-AT_30_117-VEL_12_20-UNSEL

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
   (rect-after-tick-at-left-side RECT-AT_40_100-VEL_-12_20-UNSEL)
   RECT-AT_30_117-VEL_12_20-UNSEL
   "RECT-AT_40_100-VEL_-12_20-UNSEL should move to RECT-AT_30_117-VEL_12_20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-top-side: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past top side
;; of the canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its top side touches the canvas and its center
;; remains on the moving line with only y velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-top-side RECT-AT_100_35-VEL_-12_-20-UNSEL) = RECT-AT_94_25-VEL_-12_20-UNSEL

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
   (rect-after-tick-at-top-side RECT-AT_100_35-VEL_-12_-20-UNSEL)
   RECT-AT_94_25-VEL_-12_20-UNSEL
   "RECT-AT_100_35-VEL_-12_-20-UNSEL should move to RECT-AT_94_25-VEL_-12_20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-right-side: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past right
;; side of the canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its right side touches the canvas and its center
;; remains on the moving line with only x velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-right-side RECT-AT_360_100-VEL_12_20-UNSEL) = RECT-AT_370_117-VEL_-12_20-UNSEL

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
   (rect-after-tick-at-right-side RECT-AT_360_100-VEL_12_20-UNSEL)
   RECT-AT_370_117-VEL_-12_20-UNSEL
   "RECT-AT_360_100-VEL_12_20-UNSEL should move to RECT-AT_370_117-VEL_-12_20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-at-bottom-side: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will touch or go past bottom
;; side of the canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; updated such as its bottom side touches the canvas and its center
;; remains on the moving line with only y velocity reversed from the
;; given rectangle.

;; EXAMPLES:
;; (rect-after-tick-at-bottom-side RECT-AT_100_260-VEL_12_20-UNSEL) = RECT-AT_109_275-VEL_12_-20-UNSEL

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
   (rect-after-tick-at-bottom-side RECT-AT_100_260-VEL_12_20-UNSEL)
   RECT-AT_109_275-VEL_12_-20-UNSEL
   "RECT-AT_100_260-VEL_12_20-UNSEL should move to RECT-AT_109_275-VEL_12_-20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-tick-inside-canvas: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle which will not touch or go past any side of the
;; canvas at the next tick
;; RETURNS: the rectangle after a tick, whose x and y co-ordinate are
;; moved as per their x and y velocities.
;; EXAMPLES:
;; (rect-after-tick-inside-canvas RECT-AT_200_100-VEL_-12_20-UNSEL) = RECT-AT_188_120-VEL_-12_20-UNSEL

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
   (rect-after-tick-inside-canvas RECT-AT_200_100-VEL_-12_20-UNSEL)
   RECT-AT_188_120-VEL_-12_20-UNSEL
   "RECT-AT_200_100-VEL_-12_20-UNSEL should move to RECT-AT_188_120-VEL_-12_20-UNSEL after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-x-after-tick: Rectangle NonNegInt -> NonNegInt
;; GIVEN: the unselected rectangle before the tick and the y co-ordinate of the center
;; of unselected rectangle after the tick
;; RETURNS: the x co-ordinate of the center of rectangle after the tick,
;; such as the center of the rectangle after the tick moves on the same
;; line on which the given rectangle is moving.
;; Using equation of slope: x1 = x0 + (y1 - y0)/slope here slope = vy/vx

;; EXAMPLES:
;; (rect-x-after-tick RECT-AT_100_260-VEL_12_20-UNSEL Y-MAX-LIMIT) = 109

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-x-after-tick r y1)
  (+ (rect-x r)
     (inexact->exact (round (/ (* (rect-vx r) (- y1 (rect-y r))) (rect-vy r))))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-x-after-tick RECT-AT_100_260-VEL_12_20-UNSEL Y-MAX-LIMIT)
   109
   "X co-ordinate of the center of the RECT-AT_100_260-VEL_12_20-UNSEL after tick should be 109."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-y-after-tick: Rectangle NonNegInt -> NonNegInt
;; GIVEN: the unselected rectangle before the tick and the y co-ordinate of the center
;; of unselected rectangle after the tick
;; RETURNS: the x co-ordinate of the center of rectangle after the tick,
;; such as the center of the rectangle after the tick moves on the same
;; line on which the given rectangle is moving.
;; Using equation of slope: y1 = y0 + (x1 - x0)*slope here slope = vy/vx

;; EXAMPLES:
;; (rect-y-after-tick RECT-AT_360_100-VEL_12_20-UNSEL X-MAX-LIMIT) = 117

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-y-after-tick r x1)
  (+ (rect-y r)
     (inexact->exact (round (/ (* (rect-vy r) (- x1 (rect-x r))) (rect-vx r))))))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-y-after-tick RECT-AT_360_100-VEL_12_20-UNSEL X-MAX-LIMIT)
   117
   "Y co-ordinate of the center of the RECT-AT_360_100-VEL_12_20-UNSEL after tick should be 117."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : WorldState -> Scene
;; GIVEN: the world state w
;; RETURNS: a scene that portrays the given world state.
;; EXAMPLES:
;; (world-to-scene PAUSED-WORLD-UNSEL-RECT) = IMAGE-OF-PAUSED-WORLD-UNSEL-RECT
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
;; (place-rectangle RECT-AT_200_100-VEL_-12_20-UNSEL EMPTY-CANVAS)
;;                  = (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100 EMPTY-CANVAS)
;; (place-rectangle RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
;;                  =(place-image CIRCLE-IMAGE 205 105
;;                         (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))

;; DESIGN STRATEGY: Cases on whether rectangle is selected
(define (place-rectangle r scene)
  (if
   (rect-selected? r)
   (place-rectangle-selected r scene)
   (place-rectangle-unselected r scene)))

;; place-rectangle-selected: Rectangle Scene -> Scene
;; GIVEN: a selected rectangle r and a scene 
;; RETURNS: a scene like the given one, but with the given rectangle painted
;; on it with a red circle centered at mouse co-ordinates.
;; EXAMPLES:
;; (place-rectangle-selected RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
;;                  =(place-image CIRCLE-IMAGE 205 105
;;                         (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (place-rectangle-selected r scene)
  (place-image CIRCLE-IMAGE (rect-mx r) (rect-my r)
               (place-image (rectangle-image r COLOR-RED) (rect-x r) (rect-y r)
                            scene)))

;; place-rectangle-unselected: Rectangle Scene -> Scene
;; GIVEN: an unselected rectangle r and a scene 
;; RETURNS: a scene like the given one, but with the given rectangle painted
;; on it.
;; EXAMPLES:
;; (place-rectangle-unselected RECT-AT_200_100-VEL_-12_20-UNSEL EMPTY-CANVAS)
;;                  = (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100 EMPTY-CANVAS)


;; DESIGN STRATEGY: Use template for Rectangle on r
(define (place-rectangle-unselected r scene)
  (place-image
   (rectangle-image r COLOR-BLUE)
   (rect-x r)
   (rect-y r)
   scene))

;; rectangle-image: Rectangle Color-> Image
;; GIVEN: a rectangle r and color for rectangle
;; RETURNS: an image of the rectangle
;; an outline rectangle image of given color, RECTANGLE-WIDTH pixels wide and RECTANGLE-HEIGHT
;; pixels high and current velocity of the given rectangle is displayed as a string
;; with font size = FONT-SIZE and font color = color in the center.
;; EXAMPLES:
;; (rectangle-image RECT-AT_200_100-VEL_-12_20-UNSEL COLOR-BLUE) = IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL
;; (rectangle-image RECT-AT_200_100-VEL_-12_20-SEL_205_105 COLOR-RED) = IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL
;; DESIGN STRATEGY: Combine simpler functions
(define (rectangle-image r color)
  (overlay
   (text (text-inside-rectangle r) FONT-SIZE color)
   (rectangle RECTANGLE-WIDTH RECTANGLE-HEIGHT OUTLINE color)))


;; text-inside-rectangle: Rectangle -> String
;; GIVEN: a rectangle r
;; RETURNS: a string which represents the current velocity of the given rectangle
;; EXAMPLE:
;; (text-inside-rectangle RECT-AT_200_100-VEL_-12_20-UNSEL) = "(-12, 20)"
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (text-inside-rectangle r)
  (string-append "(" (number->string (rect-vx r)) ", "
                 (number->string (rect-vy r)) ")"))

;; TESTS:
;; note: these only test whether world-to-scene calls place-image properly.
;; it doesn't check to see whether images portrayed are the right one!
(begin-for-test
  (check-equal?
   (world-to-scene PAUSED-WORLD-UNSEL-RECT)
   IMAGE-OF-PAUSED-WORLD-UNSEL-RECT
   "WorldState should be portrayed as a scene.")
  
  (check-equal?
   (place-rectangle RECT-AT_200_100-VEL_-12_20-UNSEL EMPTY-CANVAS)
   (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100 EMPTY-CANVAS)
   "Unselected rectangle should be placed on the empty canvas with its center at 200 100.")
  
  (check-equal?
   (place-rectangle RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
   (place-image CIRCLE-IMAGE 205 105
                (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))
   "Selected rectangle should be placed on the empty canvas with its center at 200 100.")
  
  (check-equal?
   (place-rectangle-selected RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
   (place-image CIRCLE-IMAGE 205 105
                (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))
   "Selected rectangle should be placed on the empty canvas with its center at 200 100.")
  
  (check-equal?
   (place-rectangle-unselected RECT-AT_200_100-VEL_-12_20-UNSEL EMPTY-CANVAS)
   (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100 EMPTY-CANVAS)
   "Unselected rectangle should be placed on the empty canvas with its center at 200 100.")
  
  (check-equal?
   (rectangle-image RECT-AT_200_100-VEL_-12_20-UNSEL COLOR-BLUE)
   IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL
   "RECT-AT_200_100-VEL_-12_20-UNSEL should be displayed as an image.")
  
  (check-equal?
   (rectangle-image RECT-AT_200_100-VEL_-12_20-SEL_205_105 COLOR-RED)
   IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL
   "RECT-AT_200_100-VEL_-12_20-SEL_205_105 should be displayed as an image.")
  
  (check-equal?
   (text-inside-rectangle RECT-AT_200_100-VEL_-12_20-UNSEL)
   "(-12, 20)"
   "RECT-AT_200_100-VEL_-12_20-UNSEL's velocities should be represented as (-12, 20)."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-key-event: WorldState KeyEvent -> WorldState
;; GIVEN: the world state w
;; RETURNS: the world state that should follow the given world state
;; after the given keyevent.
;; on space, toggle paused? -- ignore all others

;; EXAMPLES:
;; (world-after-key-event PAUSED-WORLD-UNSEL-RECT PAUSE-KEY-EVENT) = UNPAUSED-WORLD-UNSEL-RECT
;; (world-after-key-event UNPAUSED-WORLD-UNSEL-RECT PAUSE-KEY-EVENT) = PAUSED-WORLD-UNSEL-RECT
;; (world-after-key-event PAUSED-WORLD-UNSEL-RECT NON-PAUSE-KEY-EVENT) = PAUSED-WORLD-UNSEL-RECT
;; (world-after-key-event UNPAUSED-WORLD-UNSEL-RECT NON-PAUSE-KEY-EVENT) = UNPAUSED-WORLD-UNSEL-RECT

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
;; (world-with-paused-toggled PAUSED-WORLD-UNSEL-RECT PAUSE-KEY-EVENT) = UNPAUSED-WORLD-UNSEL-RECT
;; (world-with-paused-toggled UNPAUSED-WORLD-UNSEL-RECT PAUSE-KEY-EVENT) = PAUSED-WORLD-UNSEL-RECT
;; (world-with-paused-toggled PAUSED-WORLD-UNSEL-RECT NON-PAUSE-KEY-EVENT) = PAUSED-WORLD-UNSEL-RECT
;; (world-with-paused-toggled UNPAUSED-WORLD-UNSEL-RECT NON-PAUSE-KEY-EVENT) = UNPAUSED-WORLD-UNSEL-RECT

;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-with-paused-toggled w)
  (make-world
   (world-rect1 w)
   (world-rect2 w)
   (not (world-paused? w))))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event PAUSED-WORLD-UNSEL-RECT PAUSE-KEY-EVENT)
   UNPAUSED-WORLD-UNSEL-RECT
   "after pause key, a paused world should become unpaused")
  
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD-UNSEL-RECT PAUSE-KEY-EVENT)
   PAUSED-WORLD-UNSEL-RECT
   "after pause key, an unpaused world should become paused")
  
  (check-equal?
   (world-after-key-event PAUSED-WORLD-UNSEL-RECT NON-PAUSE-KEY-EVENT)
   PAUSED-WORLD-UNSEL-RECT
   "after a non-pause key, a paused world should be unchanged")
  
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD-UNSEL-RECT NON-PAUSE-KEY-EVENT)
   UNPAUSED-WORLD-UNSEL-RECT
   "after a non-pause key, an unpaused world should be unchanged"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle: NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an unselected rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).

;; EXAMPLES:
;; (new-rectangle 200 100 -12 20) = (make-rect 200 100 -12 20 false 0 0)

;; DESIGN STRATEGY: combine simpler function
(define (new-rectangle x y vx vy)
  (make-rect x y vx vy false DEFAULT-MX DEFAULT-MY))

;; TESTS:
(begin-for-test
  (check-equal?
   (new-rectangle 200 100 -12 20)
   (make-rect 200 100 -12 20 false DEFAULT-MX DEFAULT-MY)
   "An unselected rectangle centered at (200, 100) with vx = -12 and vy = 20 should be created."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-mouse-event: WorldState Integer Integer MouseEvent -> WorldState 
;; GIVEN: A world state, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world state after the given mouse
;; event.
;; EXAMPLES:
;; (world-after-mouse-event (make-world RECT-AT_200_100-VEL_-12_20-UNSEL RECT-AT_200_200-VEL_23_-14-UNSEL false)
;;  205 105 BUTTON-DOWN-EVENT) = (make-world RECT-AT_200_100-VEL_-12_20-SEL_205_105 RECT-AT_200_200-VEL_23_-14-UNSEL false)

;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-after-mouse-event w mx my mev)
  (make-world
    (rect-after-mouse-event (world-rect1 w) mx my mev)
    (rect-after-mouse-event (world-rect2 w) mx my mev)
    (world-paused? w)))

;; rect-after-mouse-event: Rectangle Integer Integer MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event
;; EXAMPLES:
;; (rect-after-mouse-event RECT-AT_200_100-VEL_-12_20-UNSEL 205 105 BUTTON-DOWN-EVENT) = RECT-AT_200_100-VEL_-12_20-SEL_205_105
;; (rect-after-mouse-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 230 130 DRAG-EVENT) = (make-rect 225 125 -12 20 true 230 130)
;; (rect-after-mouse-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 BUTTON-UP-EVENT) = RECT-AT_200_100-VEL_-12_20-UNSEL

;; DESIGN STRATEGY: Cases on MouseEvent mev
(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN-EVENT)
     (rect-after-button-down r mx my)]
    [(mouse=? mev DRAG-EVENT)
     (rect-after-drag r mx my)]
    [(mouse=? mev BUTTON-UP-EVENT)
     (rect-after-button-up r)]
    [else r]))

;; rect-after-button-down: Rectangle Integer Integer -> Rectangle
;; GIVEN: A rectangle and the x- and y-coordinates of a mouse
;; RETURNS: the rectangle following a button-down at the given location.
;; EXAMPLES:
;; (rect-after-button-down RECT-AT_200_100-VEL_-12_20-UNSEL 205 105) = RECT-AT_200_100-VEL_-12_20-SEL_205_105
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-button-down r mx my)
  (if
   (in-rect? r mx my)
   (make-rect
    (rect-x r)
    (rect-y r)
    (rect-vx r)
    (rect-vy r)
    true
    mx
    my)
   r))

;; rect-after-drag: Rectangle Integer Integer -> Rectangle
;; GIVEN: A rectangle and the x- and y-coordinates of a mouse
;; RETURNS: the rectangle following a drag at the given location
;; EXAMPLES:
;; (rect-after-drag RECT-AT_200_100-VEL_-12_20-SEL_205_105 230 130) = (make-rect 225 125 -12 20 true 230 130)
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-drag r mx my)
  (if
   (rect-selected? r)
   (make-rect
    (+ (rect-x r) (- mx (rect-mx r)))
    (+ (rect-y r) (- my (rect-my r)))
    (rect-vx r)
    (rect-vy r)
    true
    mx
    my)
   r))

;; rect-after-button-up: Rectangle -> Rectangle
;; GIVEN: a rectangle r
;; RETURNS: the rectangle following a button-up
;; EXAMPLES: 
;; (rect-after-button-up RECT-AT_200_100-VEL_-12_20-SEL_205_105) = RECT-AT_200_100-VEL_-12_20-UNSEL
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-button-up r)
  (if
   (rect-selected? r)
   (make-rect
    (rect-x r)
    (rect-y r)
    (rect-vx r)
    (rect-vy r)
    false
    DEFAULT-MY
    DEFAULT-MY)
   r))


;; TESTS:

(begin-for-test
  
  ;; button-down:
  
  ;; button-down inside rect1
  (check-equal?
   (world-after-mouse-event 
    (make-world
     RECT-AT_200_100-VEL_-12_20-UNSEL
     RECT-AT_200_200-VEL_23_-14-UNSEL
     false)
    205
    105
    BUTTON-DOWN-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-SEL_205_105
    RECT-AT_200_200-VEL_23_-14-UNSEL
    false)
   "button down inside rect1 should select it but didn't")
  
  
  ;; button-down inside rect2
  (check-equal?
   (world-after-mouse-event 
    (make-world
     RECT-AT_200_100-VEL_-12_20-UNSEL
     RECT-AT_200_200-VEL_23_-14-UNSEL
     false)
    205
    205
    BUTTON-DOWN-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-SEL_205_205
    false)
   "button down inside rect2 should select it but didn't")
  
  ;; button-down not inside any rectangle
  (check-equal?
   (world-after-mouse-event 
    (make-world
     RECT-AT_200_100-VEL_-12_20-UNSEL
     RECT-AT_200_200-VEL_23_-14-UNSEL
     false)
    50
    50
    BUTTON-DOWN-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-UNSEL
    false)
   "button down outside any rect should leave world unchanged, but didn't")
  
  ;; drag:
  
  ;; no rectangle selected: drag should not change anything
  (check-equal?
   (world-after-mouse-event
    (make-world
     RECT-AT_200_100-VEL_-12_20-UNSEL
     RECT-AT_200_200-VEL_23_-14-UNSEL
     false)
    100
    50
    DRAG-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-UNSEL
    false)
   "drag with no rect selected didn't leave world unchanged")
  
  ;; rect1 selected
  (check-equal?
   (world-after-mouse-event
    (make-world
     RECT-AT_200_100-VEL_-12_20-SEL_205_105
     RECT-AT_200_200-VEL_23_-14-UNSEL
     false)
    230 130
    DRAG-EVENT)
   (make-world
    (make-rect 225 125 -12 20 true 230 130)
    RECT-AT_200_200-VEL_23_-14-UNSEL
    false)
   "drag when rect1 is selected should just move rect1, but didn't")
  
  ;; rect2 selected
  (check-equal?
   (world-after-mouse-event
    (make-world
     RECT-AT_200_100-VEL_-12_20-UNSEL
     RECT-AT_200_200-VEL_23_-14-SEL_205_205
     false)
    230 230
    DRAG-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    (make-rect 225 225 23 -14 true 230 230)
    false)
   "drag when rect2 is selected should just move rect2, but didn't")
  
  ;; button-up:
  
  ;; unselect rect1
  (check-equal?
   (world-after-mouse-event
    (make-world
     RECT-AT_200_100-VEL_-12_20-SEL_205_105
     RECT-AT_200_200-VEL_23_-14-UNSEL
     true)
    205 105
    BUTTON-UP-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-UNSEL
    true)
   "button-up failed to unselect rect1")
  
  ;; unselect rect2
  (check-equal?
   (world-after-mouse-event
    (make-world
     RECT-AT_200_100-VEL_-12_20-UNSEL
     RECT-AT_200_200-VEL_23_-14-SEL_205_205
     true)
    205 205
    BUTTON-UP-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-UNSEL
    true)
   "button-up failed to unselect rect2")
  
  ;; unselect both
  (check-equal?
   (world-after-mouse-event
    (make-world
     RECT-AT_200_100-VEL_-12_20-UNSEL
     RECT-AT_200_200-VEL_23_-14-UNSEL
     true)
    50 50 
    BUTTON-UP-EVENT)
   (make-world
    RECT-AT_200_100-VEL_-12_20-UNSEL
    RECT-AT_200_200-VEL_23_-14-UNSEL
    true)
   "button-up with two unselected rects failed.")
  
  ;; tests for other mouse events
  
  (check-equal?
   (world-after-mouse-event
    UNPAUSED-WORLD-UNSEL-RECT 
    50 50
    MOVE-EVENT)
   UNPAUSED-WORLD-UNSEL-RECT
   "other mouse events should leave the world unchanged, but didn't"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; in-rect?: Rectangle Integer Integer -> Rectangle
;; GIVEN: the rectangle r and x & y co-ordinates
;; RETURNS: true if the given coordinate is inside the given rectangle.
;; EXAMPLES:
;; (in-rect? RECT-AT_200_100-VEL_-12_20-UNSEL 205 105) = true
;; (in-rect? RECT-AT_200_100-VEL_-12_20-UNSEL 250 105) = false

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (in-rect? r mx my)
  (and
    (<= 
      (- (rect-x r) HALF-RECTANGLE-WIDTH)
      mx
      (+ (rect-x r) HALF-RECTANGLE-WIDTH))
    (<= 
      (- (rect-y r) HALF-RECTANGLE-HEIGHT)
      my
      (+ (rect-y r) HALF-RECTANGLE-HEIGHT))))

;; TESTS:

(begin-for-test
  (check-equal?
   (in-rect? RECT-AT_200_100-VEL_-12_20-UNSEL 205 105)
   true
   "(205,105) should be inside of RECT-AT_200_100-VEL_-12_20-UNSEL.")
  
  (check-equal?
   (in-rect? RECT-AT_200_100-VEL_-12_20-UNSEL 250 105)
   false
   "(250,105) should be outside of RECT-AT_200_100-VEL_-12_20-UNSEL."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (screensaver 0.5)