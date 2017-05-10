;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-3.rkt: moving and draggable ranctangles.  
;; User can create new rectangle with "n" key and increase the velocity of
;; the rectangle in the specified direction (up, down, left, or right) using
;; arrow keys after selecting the rectangle.

;; Each rectangle moves as per its velocity in x and y directions and it
;; bounces smoothly off the edge of the canvas. The user can pause/unpause
;; the entire scene using space bar.

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
 world-rects
 world-paused?
 new-rectangle
 rect-x
 rect-y
 rect-vx
 rect-vy
 world-after-mouse-event
 rect-after-mouse-event
 rect-selected?
 rect-after-key-event
 )

;; check the location of file for automated testing
;; (check-location "04" "screensaver-3.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))

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

;; initial x and y velocities of two rectangles and paused & selected flag
(define INIT-VX 0)
(define INIT-VY 0)
(define INIT-PAUSED-TRUE true)
(define INIT-SELECTED-FALSE false)

;; default mouse co-ordinates
(define DEFAULT-MX 0)
(define DEFAULT-MY 0)

;; increase in velocity on arrow key
(define VEL-INCREMENT 2)

;; bracket and comma for velocity string creation
(define START-BRACKET "(")
(define END-BRACKET ")")
(define COMMA ", ")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; DATA DEFINITIONS:

(define-struct rect (x y vx vy selected? mx my))
;; A Rectangle is a
;; (make-rect NonNegInt NonNegInt Integer Integer Boolean NonNegInt NonNegInt)
;; INTERPRETATION: 
;; x and y are the co-ordinates of the center point of the rectangle, 
;; vx and vy are the velocities of the rectangle in x and y direction
;;     respectively in pixels/tick.
;; selected? describes whether or not the rectangle is selected by mouse.
;; mx and my are the current x and y co-ordinates of the mouse if rectangle
;;     is selected else they are DEFAULT-MX & DEFAULT-MY.

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
(define RECT-AT_30_120-VEL_12_20-UNSEL (make-rect 30 120 12 20 false 0 0))

(define RECT-AT_100_35-VEL_-12_-20-UNSEL (make-rect 100 35 -12 -20 false 0 0))
(define RECT-AT_88_25-VEL_-12_20-UNSEL (make-rect 88 25 -12 20 false 0 0))

(define RECT-AT_360_100-VEL_12_20-UNSEL (make-rect 360 100 12 20 false 0 0))
(define RECT-AT_370_120-VEL_-12_20-UNSEL (make-rect 370 120 -12 20 false 0 0))

(define RECT-AT_100_260-VEL_12_20-UNSEL (make-rect 100 260 12 20 false 0 0))
(define RECT-AT_112_275-VEL_12_-20-UNSEL (make-rect 112 275 12 -20 false 0 0))

(define RECT-AT_200_100-VEL_-12_20-SEL_205_105 (make-rect 200 100 -12 20 true 205 105))
(define RECT-AT_200_200-VEL_23_-14-SEL_205_205 (make-rect 200 200 23 -14 true 205 205))

(define NEW-RECTANGLE-AT-CENTER-OF-CANVAS
  (make-rect HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT INIT-VX INIT-VY false 0 0))


;; A ListOfRectangle is either
;; -- empty
;; -- (cons Rectangle ListOfRectangle)
;; TEMPLATE:
#|
lor-fn : ListOfRectangle -> ??
(define (lor-fn lor)
  (cond
    [(empty? lor) ...]
    [else (...
           (rect-fn (first lor))
           (lor-fn (rest lor)))]))
|#

;; examples of ListOfRectangle, for testing
(define RECTANGLE-LIST (list RECT-AT_200_100-VEL_-12_20-UNSEL
                             RECT-AT_200_200-VEL_23_-14-UNSEL))

(define-struct world (rects paused?))
;; A WorldState is a (make-world ListOfRectangle Boolean)
;; INTERPRETATION:
;; rects is a list of rectangles in the world state
;; paused? describes whether or not the world is paused

;; TEMPLATE:
;; world-fn : WorldState -> ??
#|
(define (world-fn w)
  (...
   (world-rects w)
   (world-paused? w)))
|#

;; examples of worlds, for testing

(define PAUSED-WORLD-UNSEL-RECT
  (make-world RECTANGLE-LIST true))

(define UNPAUSED-WORLD-UNSEL-RECT
  (make-world RECTANGLE-LIST false))

(define UNPAUSED-WORLD-UNSEL-RECT-AFTER-TICK
  (make-world (list RECT-AT_188_120-VEL_-12_20-UNSEL
                    RECT-AT_223_186-VEL_23_-14-UNSEL)
              false))


;; examples of KeyEvents, for testing
(define SPACE-KEY-EVENT " ")
(define N-KEY-EVENT "n")
(define LEFT-KEY-EVENT "left")
(define RIGHT-KEY-EVENT "right")
(define UP-KEY-EVENT "up")
(define DOWN-KEY-EVENT "down")
(define Q-KEY-EVENT "q")


;; examples of MouseEvents, for testing
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")
(define MOVE-EVENT "move")

;; END DATA DEFINITIONS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world as specified in the problem set
;; EXAMPLES:
;; (initial-world ANY-VALUE) = (make-world empty INIT-PAUSED-TRUE)

;; DESIGN STRATEGY: combine simpler functions
(define (initial-world n)
  (make-world empty INIT-PAUSED-TRUE))

;; TESTS:
(begin-for-test
  (check-equal?
   (initial-world ANY-VALUE)
   (make-world empty INIT-PAUSED-TRUE)
   "Initial world should be created without any rectangle."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-tick: WorldState -> WorldState
;; GIVEN: the world state w
;; RETURNS: the world state that should follow the given world state
;; after a tick.

;; EXAMPLES:
;; (world-after-tick PAUSED-WORLD-UNSEL-RECT) = PAUSED-WORLD-UNSEL-RECT
;; (world-after-tick UNPAUSED-WORLD-UNSEL-RECT) = UNPAUSED-WORLD-UNSEL-RECT-AFTER-TICK

;; DESIGN STRATEGY: Divide into cases on (world-paused? w)

(define (world-after-tick w)
  (if (world-paused? w)
      w
      (make-world
       (rects-after-tick (world-rects w))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rects-after-tick: ListOfRectangle -> ListOfRectangle
;; GIVEN: the list of rectangles rects
;; RETURNS: the list of rectangles that should follow the given list of rectangles
;; after a tick.
;; EXAMPLES:
#|
(rects-after-tick
 (list RECT-AT_200_100-VEL_-12_20-UNSEL RECT-AT_200_100-VEL_-12_20-SEL_205_105))
  = (list RECT-AT_188_120-VEL_-12_20-UNSEL RECT-AT_200_100-VEL_-12_20-SEL_205_105)
|#

;; DESIGN STRATEGY: Use template for ListOfRectangle on rects
(define (rects-after-tick rects)
 (cond
    [(empty? rects) empty]
    [else (cons
           (rect-after-tick (first rects))
           (rects-after-tick (rest rects)))]))

;; rect-after-tick: Rectangle -> Rectangle
;; GIVEN: the rectangle r
;; RETURNS: the rectangle that should move as per x and y velocities of given
;; rectangle after a tick if the given rectangle is unselected else returns the
;; given rectangle.

;; EXAMPLES:
#|
(rect-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL) = RECT-AT_188_120-VEL_-12_20-UNSEL
(rect-after-tick RECT-AT_200_100-VEL_-12_20-SEL_205_105)
  = RECT-AT_200_100-VEL_-12_20-SEL_205_105
|#
;; DESIGN STRATEGY: Divide into cases on (rect-selected? rect)
(define (rect-after-tick rect)
  (if
   (rect-selected? rect)
   rect
   (rect-unsel-after-tick rect)))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL)
   RECT-AT_188_120-VEL_-12_20-UNSEL
   "RECT-AT_200_100-VEL_-12_20-UNSEL should move to (188,120) after the tick.")

  (check-equal?
   (rect-after-tick RECT-AT_200_100-VEL_-12_20-SEL_205_105)
   RECT-AT_200_100-VEL_-12_20-SEL_205_105
   "RECT-AT_200_100-VEL_-12_20-SEL_205_105 should not move after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-unsel-after-tick: Rectangle -> Rectangle
;; GIVEN: the unselected rectangle r
;; RETURNS: the rectangle that should move as per x and y velocities of given
;; rectangle after a tick.
;; EXAMPLES:
#|
(rect-unsel-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL)=RECT-AT_188_120-VEL_-12_20-UNSEL
(rect-unsel-after-tick RECT-AT_40_30-VEL_-12_-20-UNSEL)=RECT-AT_30_25-VEL_12_20-UNSEL
(rect-unsel-after-tick RECT-AT_365_30-VEL_12_-20-UNSEL)=RECT-AT_370_25-VEL_-12_20-UNSEL
(rect-unsel-after-tick RECT-AT_365_265-VEL_12_20-UNSEL)=RECT-AT_370_275-VEL_-12_-20-UNSEL
(rect-unsel-after-tick RECT-AT_40_270-VEL_-12_20-UNSEL)=RECT-AT_30_275-VEL_12_-20-UNSEL
(rect-unsel-after-tick RECT-AT_40_100-VEL_-12_20-UNSEL)=RECT-AT_30_120-VEL_12_20-UNSEL
(rect-unsel-after-tick RECT-AT_100_35-VEL_-12_-20-UNSEL)=RECT-AT_88_25-VEL_-12_20-UNSEL
(rect-unsel-after-tick RECT-AT_360_100-VEL_12_20-UNSEL)=RECT-AT_370_120-VEL_-12_20-UNSEL
(rect-unsel-after-tick RECT-AT_100_260-VEL_12_20-UNSEL)=RECT-AT_112_275-VEL_12_-20-UNSEL
|#
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-unsel-after-tick r)
  (make-rect (rect-x-after-tick r)
             (rect-y-after-tick r)
             (rect-vx-after-tick r)
             (rect-vy-after-tick r)
             (rect-selected? r)
             (rect-mx r)
             (rect-my r)))

;; rect-x-after-tick: Rectangle -> NonNegInt
;; GIVEN: the unselected rectangle r
;; RETURNS: the x coordinate of the rectangle after a tick.
;; EXAMPLES:
#|
(rect-x-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL) = 188
(rect-x-after-tick RECT-AT_40_30-VEL_-12_-20-UNSEL) = 30
(rect-x-after-tick RECT-AT_365_30-VEL_12_-20-UNSEL) = 370
|#
;; DESIGN STRATEGY: Cases on whether Rectangle touch or go past any vertical side
;; of the canvas on the next tick
(define (rect-x-after-tick r)
  (cond
    [(rect-touch-or-go-past-left-side? r) X-MIN-LIMIT]
    [(rect-touch-or-go-past-right-side? r) X-MAX-LIMIT]
    [else (+ (rect-x r) (rect-vx r))]))

;; rect-y-after-tick: Rectangle -> NonNegInt
;; GIVEN: the unselected rectangle r
;; RETURNS: the y coordinate of the rectangle after a tick.
;; EXAMPLES:
#|
(rect-y-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL) = 120
(rect-y-after-tick RECT-AT_40_30-VEL_-12_-20-UNSEL) = 25
(rect-y-after-tick RECT-AT_365_265-VEL_12_-20-UNSEL) = 275
|#
;; DESIGN STRATEGY: Cases on whether Rectangle touch or go past any horizontal side
;; of the canvas on the next tick
(define (rect-y-after-tick r)
  (cond
    [(rect-touch-or-go-past-top-side? r) Y-MIN-LIMIT]
    [(rect-touch-or-go-past-bottom-side? r) Y-MAX-LIMIT]
    [else (+ (rect-y r) (rect-vy r))]))


;; rect-vx-after-tick: Rectangle -> Integer
;; GIVEN: the unselected rectangle r
;; RETURNS: the x velocity of the recatngle after a tick.
;; EXAMPLES:
#|
(rect-vx-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL) = -12
(rect-vx-after-tick RECT-AT_40_30-VEL_-12_-20-UNSEL) = 12
(rect-vx-after-tick RECT-AT_365_30-VEL_12_-20-UNSEL) = 12
|#
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-vx-after-tick r)
  (if (or (rect-touch-or-go-past-left-side? r) (rect-touch-or-go-past-right-side? r))
      (- (rect-vx r))
      (rect-vx r)))

;; rect-vy-after-tick: Rectangle -> Integer
;; GIVEN: the unselected rectangle r
;; RETURNS: the y velocity of the recatngle after a tick.
;; EXAMPLES:
#|
(rect-vy-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL) = 20
(rect-vy-after-tick RECT-AT_40_30-VEL_-12_20-UNSEL) = -20
(rect-vy-after-tick RECT-AT_365_265-VEL_-12_20-UNSEL) = -20
|#
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-vy-after-tick r)
  (if (or (rect-touch-or-go-past-top-side? r) (rect-touch-or-go-past-bottom-side? r))
      (- (rect-vy r))
      (rect-vy r)))

;; TESTS:
(begin-for-test
  (check-equal?
   (rect-after-tick RECT-AT_200_100-VEL_-12_20-UNSEL)
   RECT-AT_188_120-VEL_-12_20-UNSEL
   "RECT-AT_200_100-VEL_-12_20-UNSEL should move to (188,120) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_40_30-VEL_-12_-20-UNSEL)
   RECT-AT_30_25-VEL_12_20-UNSEL
   "RECT-AT_40_30-VEL_-12_-20-UNSEL should move to (30,25) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_365_30-VEL_12_-20-UNSEL)
   RECT-AT_370_25-VEL_-12_20-UNSEL
   "RECT-AT_365_30-VEL_12_-20-UNSEL should move to (370,25) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_365_265-VEL_12_20-UNSEL)
   RECT-AT_370_275-VEL_-12_-20-UNSEL
   "RECT-AT_365_265-VEL_12_20-UNSEL should move to (370,275) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_40_270-VEL_-12_20-UNSEL)
   RECT-AT_30_275-VEL_12_-20-UNSEL
   "RECT-AT_40_270-VEL_-12_20-UNSEL should move to (30,275) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_40_100-VEL_-12_20-UNSEL)
   RECT-AT_30_120-VEL_12_20-UNSEL
   "RECT-AT_40_100-VEL_-12_20-UNSEL should move to (30,120) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_100_35-VEL_-12_-20-UNSEL)
   RECT-AT_88_25-VEL_-12_20-UNSEL
   "RECT-AT_100_35-VEL_-12_-20-UNSEL should move to (-12,20) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_360_100-VEL_12_20-UNSEL)
   RECT-AT_370_120-VEL_-12_20-UNSEL
   "RECT-AT_360_100-VEL_12_20-UNSEL should move to (370,120) after the tick.")
  
  (check-equal?
   (rect-after-tick RECT-AT_100_260-VEL_12_20-UNSEL)
   RECT-AT_112_275-VEL_12_-20-UNSEL
   "RECT-AT_100_260-VEL_12_20-UNSEL should move to (112,275) after the tick."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
   "RECT-AT_40_100-VEL_-12_20-UNSEL should touch or go past left side.")
  
  (check-equal?
   (rect-touch-or-go-past-left-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past left side.")
  
  (check-equal?
   (rect-touch-or-go-past-top-side? RECT-AT_100_35-VEL_-12_-20-UNSEL)
   true
   "RECT-AT_100_35-VEL_-12_-20-UNSEL should touch or go past top side.")
  
  (check-equal?
   (rect-touch-or-go-past-top-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past top side.")
  
  (check-equal?
   (rect-touch-or-go-past-right-side? RECT-AT_360_100-VEL_12_20-UNSEL)
   true
   "RECT-AT_360_100-VEL_12_20-UNSEL should touch or go past right side.")
  
  (check-equal?
   (rect-touch-or-go-past-right-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past right side.")
  
  (check-equal?
   (rect-touch-or-go-past-bottom-side? RECT-AT_100_260-VEL_12_20-UNSEL)
   true
   "RECT-AT_100_260-VEL_12_20-UNSEL should touch or go past bottom side.")
  
  (check-equal?
   (rect-touch-or-go-past-bottom-side? RECT-AT_200_100-VEL_-12_20-UNSEL)
   false
   "RECT-AT_200_100-VEL_-12_20-UNSEL should not touch or go past bottom side."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : WorldState -> Scene
;; GIVEN: the world state w
;; RETURNS: a scene that portrays the given world state.
;; EXAMPLES:
;; (world-to-scene PAUSED-WORLD-UNSEL-RECT) = IMAGE-OF-PAUSED-WORLD-UNSEL-RECT
;; DESIGN STRATEGY: Use template for ListOfRectangle on (world-rects w)
(define (world-to-scene w)
  (cond
    [(empty? (world-rects w)) EMPTY-CANVAS]
    [else (place-rectangle
           (first (world-rects w))
           (world-to-scene (make-world (rest (world-rects w))
                                       (world-paused? w))))]))

;; place-rectangle: Rectangle Scene -> Scene
;; GIVEN: a rectangle r and a scene
;; RETURNS: a scene like the given one, but with the given rectangle painted
;; on it.
;; EXAMPLES:
#|
(place-rectangle RECT-AT_200_100-VEL_-12_20-UNSEL EMPTY-CANVAS)
  = (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100 EMPTY-CANVAS)

(place-rectangle RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
  = (place-image
     CIRCLE-IMAGE
     205 105
     (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))
|#
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
#|
(place-rectangle-selected RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
  = (place-image
     CIRCLE-IMAGE
     205 105
     (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))
|#

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
#|
(place-rectangle-unselected RECT-AT_200_100-VEL_-12_20-UNSEL EMPTY-CANVAS)
  = (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100 EMPTY-CANVAS)
|#
;; DESIGN STRATEGY: Use template for Rectangle on r
(define (place-rectangle-unselected r scene)
  (place-image
   (rectangle-image r COLOR-BLUE)
   (rect-x r)
   (rect-y r)
   scene))

;; rectangle-image: Rectangle Color-> Image
;; GIVEN: a rectangle r and color for rectangle
;; RETURNS: an image of the rectangle, an outline rectangle image of given color,
;; RECTANGLE-WIDTH pixels wide and RECTANGLE-HEIGHT pixels high and current velocity
;; of the given rectangle is displayed as a string with font size = FONT-SIZE and
;; font color = color in the center.

;; EXAMPLES:
#|
(rectangle-image RECT-AT_200_100-VEL_-12_20-UNSEL COLOR-BLUE)
  = IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL

(rectangle-image RECT-AT_200_100-VEL_-12_20-SEL_205_105 COLOR-RED)
  = IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL
|#

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
  (string-append START-BRACKET
                 (number->string (rect-vx r)) COMMA
                 (number->string (rect-vy r)) END-BRACKET))

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
   "Unselected rectangle should be placed on the empty canvas, center at 200 100.")
  
  (check-equal?
   (place-rectangle RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
   (place-image CIRCLE-IMAGE 205 105
                (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))
   "Selected rectangle should be placed on the empty canvas, center at 200 100.")
  
  (check-equal?
   (place-rectangle-selected RECT-AT_200_100-VEL_-12_20-SEL_205_105 EMPTY-CANVAS)
   (place-image CIRCLE-IMAGE 205 105
                (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-SEL 200 100 EMPTY-CANVAS))
   "Selected rectangle should be placed on the empty canvas, center at 200 100.")
  
  (check-equal?
   (place-rectangle-unselected RECT-AT_200_100-VEL_-12_20-UNSEL EMPTY-CANVAS)
   (place-image IMAGE-OF-RECT-AT_200_100-VEL_-12_20-UNSEL 200 100 EMPTY-CANVAS)
   "Unselected rectangle should be placed on the empty canvas, center at 200 100.")
  
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-key-event: WorldState KeyEvent -> WorldState
;; GIVEN: the world state w and Key event kev
;; RETURNS: the world state that should follow the given world state after the
;; given key event.
;; on space: toggle paused?
;; on "n" key: add new rectangle in current world state
;; on arrow keys: increase the velocity of the selected rectangle in the specified
;; direction (up, down, left, or right) by 2 pixels/tick. ignore all other keys.
;; EXAMPLES:
#|
(world-after-key-event PAUSED-WORLD-UNSEL-RECT SPACE-KEY-EVENT)
= UNPAUSED-WORLD-UNSEL-RECT

(world-after-key-event (make-world empty true) N-KEY-EVENT)
= (make-world (list NEW-RECTANGLE-AT-CENTER-OF-CANVAS) true)

(world-after-key-event PAUSED-WORLD-UNSEL-RECT Q-KEY-EVENT)
= PAUSED-WORLD-UNSEL-RECT

(world-after-key-event
 (make-world (list RECT-AT_200_100-VEL_-12_20-SEL_205_105) true) LEFT-KEY-EVENT)
= (make-world (list (make-rect 200 100 -14 20 tru 205 105)) true)
|#
;; DESIGN STRATEGY: Cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev SPACE-KEY-EVENT) (world-after-space-key-event w)]
    [(key=? kev N-KEY-EVENT) (world-after-n-key-event w)]
    [(or (key=? kev LEFT-KEY-EVENT)
         (key=? kev RIGHT-KEY-EVENT)
         (key=? kev UP-KEY-EVENT)
         (key=? kev DOWN-KEY-EVENT)) (world-after-other-key-events w kev)]
    [else w]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-space-key-event: WorldState -> WorldState
;; GIVEN: the world state w
;; RETURNS: a world state just like the given one, but with paused? toggled
;; EXAMPLES:
#|
(world-after-space-key-event PAUSED-WORLD-UNSEL-RECT SPACE-KEY-EVENT)
= UNPAUSED-WORLD-UNSEL-RECT

(world-after-space-key-event UNPAUSED-WORLD-UNSEL-RECT SPACE-KEY-EVENT)
= PAUSED-WORLD-UNSEL-RECT

(world-after-space-key-event PAUSED-WORLD-UNSEL-RECT Q-KEY-EVENT)
= PAUSED-WORLD-UNSEL-RECT

(world-after-space-key-event UNPAUSED-WORLD-UNSEL-RECT Q-KEY-EVENT)
= UNPAUSED-WORLD-UNSEL-RECT
|#
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-after-space-key-event w)
  (make-world
   (world-rects w)
   (not (world-paused? w))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-n-key-event: WorldState -> WorldState
;; GIVEN: the world state w
;; RETURNS: a world state after adding a new rectangle in the given world state.
;; EXAMPLES:
#|
(world-after-n-key-event (make-world empty true))
= (make-world (list NEW-RECTANGLE-AT-CENTER-OF-CANVAS) true)
|#
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-after-n-key-event w)
  (make-world
   (cons (new-rectangle HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT INIT-VX INIT-VY)
         (world-rects w))
   (world-paused? w)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-other-key-events: WorldState KeyEvent -> WorldState
;; GIVEN: the world state w and one of the arrow key event kev
;; RETURNS: the world state that should follow the given world state after the given
;; key event.
;; EXAMPLES:
#|
(world-after-other-key-events
 (make-world (list RECT-AT_200_100-VEL_-12_20-SEL_205_105) true) LEFT-KEY-EVENT)
= (make-world (list (make-rect 200 100 -14 20 true 205 105)) true)
|#
;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-after-other-key-events w kev)
  (make-world
   (rects-after-key-event (world-rects w) kev)
   (world-paused? w)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rects-after-key-event: ListOfRectangle KeyEvent -> ListOfRectangle
;; GIVEN: the list of rectangle rects of some world state and one of the arrow
;; key event kev
;; RETURNS: the list of rectangles where each rectangle should follow the rectangle
;; in the given list after the given key event.
;; EXAMPLES:
#|
(rects-after-key-event (list RECT-AT_200_100-VEL_-12_20-SEL_205_105) LEFT-KEY-EVENT)
= (list (make-rect 200 100 -14 20 true 205 105))
|#
;; DESIGN STRATEGY: Use template for ListOfRectangle on rects
(define (rects-after-key-event rects kev)
  (cond
    [(empty? rects) empty]
    [else (cons
           (rect-after-key-event (first rects) kev)
           (rects-after-key-event (rest rects) kev))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-after-key-event: Rectangle KeyEvent -> Rectangle
;; GIVEN: the rectangle rect and one of the arrow key event kev
;; RETURNS: the rectangle that should follow the given rectangle after the given
;; key event.
;; EXAMPLES:
#|
(rect-after-key-event RECT-AT_200_100-VEL_-12_20-UNSEL LEFT-KEY-EVENT)
= RECT-AT_200_100-VEL_-12_20-UNSEL

(rect-after-key-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 LEFT-KEY-EVENT)
= (make-rect 200 100 -14 20 true 205 105)
|#
;; DESIGN STRATEGY: Cases on whether Rectangle is selected
(define (rect-after-key-event rect kev)
  (if (rect-selected? rect)
      (rect-selected-after-key-event rect kev)
      rect))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-selected-after-key-event: Rectangle KeyEvent -> Rectangle
;; GIVEN: the selected rectangle rect and one of the arrow key event kev
;; RETURNS: the rectangle that should follow the given rectangle after the
;; given key event.
;; on arrow keys, increase the velocity of the selected rectangle in the specified
;; direction (up, down, left, or right) by VEl-INCREMENT pixels/tick.
;; ignore all other keys.

;; EXAMPLES:
#|
(rect-selected-after-key-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 LEFT-KEY-EVENT)
= (make-rect 200 100 -14 20 true 205 105)

(rect-selected-after-key-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 RIGHT-KEY-EVENT)
= (make-rect 200 100 -10 20 true 205 105)

(rect-selected-after-key-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 UP-KEY-EVENT)
= (make-rect 200 100 -12 18 true 205 105)

(rect-selected-after-key-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 DOWN-KEY-EVENT)
= (make-rect 200 100 -12 22 true 205 105)
|#

;; DESIGN STRATEGY: Use template for Rectangle on rect
(define (rect-selected-after-key-event rect kev)
  (make-rect (rect-x rect)
             (rect-y rect)
             (rect-vx-after-key-event (rect-vx rect) kev)
             (rect-vy-after-key-event (rect-vy rect) kev)
             (rect-selected? rect)
             (rect-mx rect)
             (rect-my rect)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rect-vx-after-key-event: Integer KeyEvent -> Integer
;; GIVEN: the velocity of some rectangle in x direction vx and key event kev
;; RETURNS: the velocity in x direction that should follow the given key event.
;; increase vx on LEFT-KEY-EVENT and RIGHT-KEY-EVENT in specific direction, ignore
;; other key events
;; EXAMPLES:
#|
(rect-vx-after-key-event 20 LEFT-KEY-EVENT) = 18
(rect-vx-after-key-event -20 LEFT-KEY-EVENT) = -22
(rect-vx-after-key-event 20 RIGHT-KEY-EVENT) = 22
(rect-vx-after-key-event -20 RIGHT-KEY-EVENT) = -18
(rect-vx-after-key-event 20 Q-KEY-EVENT) = 20
|#

;; DESIGN STRATEGY: Cases on KeyEvent kev
(define (rect-vx-after-key-event vx kev)
  (cond
    [(key=? kev LEFT-KEY-EVENT) (- vx VEL-INCREMENT)]
    [(key=? kev RIGHT-KEY-EVENT) (+ vx VEL-INCREMENT)]
    [else vx]))


;; rect-vy-after-key-event: Integer KeyEvent -> Integer
;; GIVEN: the velocity of some rectangle in y direction vy and key event kev
;; RETURNS: the velocity in y direction that should follow the given key event.
;; increase vy on UP-KEY-EVENT and DOWN-KEY-EVENT in specific direction, ignore
;; other key events
;; EXAMPLES:
#|
(rect-vy-after-key-event 20 UP-KEY-EVENT) = 18
(rect-vy-after-key-event -20 UP-KEY-EVENT) = -22
(rect-vy-after-key-event 20 DOWN-KEY-EVENT) = 22
(rect-vy-after-key-event -20 DOWN-KEY-EVENT) = -18
(rect-vy-after-key-event 20 Q-KEY-EVENT) = 20
|#

;; DESIGN STRATEGY: Cases on KeyEvent kev
(define (rect-vy-after-key-event vy kev)
  (cond
    [(key=? kev UP-KEY-EVENT) (- vy VEL-INCREMENT)]
    [(key=? kev DOWN-KEY-EVENT) (+ vy VEL-INCREMENT)]
    [else vy]))


;;TESTS:

(begin-for-test

  (check-equal?
   (world-after-key-event PAUSED-WORLD-UNSEL-RECT SPACE-KEY-EVENT)
   UNPAUSED-WORLD-UNSEL-RECT
   "after pause key, a paused world should become unpaused")
  
  (check-equal?
   (world-after-key-event UNPAUSED-WORLD-UNSEL-RECT SPACE-KEY-EVENT)
   PAUSED-WORLD-UNSEL-RECT
   "after pause key, an unpaused world should become paused")
  
  (check-equal?
   (world-after-key-event (make-world empty true) N-KEY-EVENT)
   (make-world (list NEW-RECTANGLE-AT-CENTER-OF-CANVAS) true)
   "After n key, a new rectangle should be added in world")
  
  (check-equal?
   (world-after-key-event (make-world
                           (list RECT-AT_200_100-VEL_-12_20-SEL_205_105)
                           true) LEFT-KEY-EVENT)
   (make-world (list (make-rect 200 100 -14 20 true 205 105)) true)
   "After left key, vx should be descreased of selected rectangle.")
  
  (check-equal?
   (world-after-key-event PAUSED-WORLD-UNSEL-RECT Q-KEY-EVENT)
   PAUSED-WORLD-UNSEL-RECT
   "After q key, there should be no change in world.")

  (check-equal?
   (rect-after-key-event RECT-AT_200_100-VEL_-12_20-UNSEL LEFT-KEY-EVENT)
   RECT-AT_200_100-VEL_-12_20-UNSEL
   "after left key, an unselected rectangle should not change.")
  
  (check-equal?
   (rect-after-key-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 LEFT-KEY-EVENT)
   (make-rect 200 100 -14 20 true 205 105)
   "after left key, a select rectangle should be changed.")  
  
  (check-equal? (rect-vx-after-key-event 20 LEFT-KEY-EVENT) 18
   "LEFT-KEY-EVENT should decrease vx from 20 to 18.")
  
  (check-equal? (rect-vx-after-key-event -20 LEFT-KEY-EVENT) -22
   "LEFT-KEY-EVENT should decrease vx from -20 to -22.")
  
  (check-equal? (rect-vx-after-key-event 20 RIGHT-KEY-EVENT) 22
   "RIGHT-KEY-EVENT should increase vx from 20 to 22.")
  
  (check-equal? (rect-vx-after-key-event -20 RIGHT-KEY-EVENT) -18
   "RIGHT-KEY-EVENT should increase vx from -20 to -18.")
  
  (check-equal? (rect-vx-after-key-event 20 Q-KEY-EVENT) 20
   "Q-KEY-EVENT should not change vx.")

  (check-equal? (rect-vy-after-key-event 20 UP-KEY-EVENT) 18
   "UP-KEY-EVENT should decrease vy from 20 to 18.")
  
  (check-equal? (rect-vy-after-key-event -20 UP-KEY-EVENT) -22
   "UP-KEY-EVENT should decrease vy from -20 to -22.")
  
  (check-equal? (rect-vy-after-key-event 20 DOWN-KEY-EVENT) 22
   "DOWN-KEY-EVENT should increase vy from 20 to 22.")
  
  (check-equal? (rect-vy-after-key-event -20 DOWN-KEY-EVENT) -18
   "DOWN-KEY-EVENT should increase vy from -20 to -18.")
  
  (check-equal? (rect-vy-after-key-event 20 Q-KEY-EVENT) 20
   "Q-KEY-EVENT should not change vy."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; new-rectangle: NonNegInt NonNegInt Int Int -> Rectangle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: an unselected rectangle centered at (x,y), which will travel with
;; velocity (vx, vy).

;; EXAMPLES:
;; (new-rectangle 200 100 -12 20) = (make-rect 200 100 -12 20 false 0 0)

;; DESIGN STRATEGY: combine simpler function
(define (new-rectangle x y vx vy)
  (make-rect x
             y
             vx
             vy
             false
             DEFAULT-MX
             DEFAULT-MY))

;; TESTS:
(begin-for-test
  (check-equal?
   (new-rectangle 200 100 -12 20)
   (make-rect 200 100 -12 20 false DEFAULT-MX DEFAULT-MY)
   "An unsel rect centered at (200,100) with vx = -12 and vy = 20 should be created."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-mouse-event: WorldState Integer Integer MouseEvent -> WorldState 
;; GIVEN: A world state, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world state after the given mouse
;; event.
;; EXAMPLES:
#|
(world-after-mouse-event (make-world RECT-AT_200_100-VEL_-12_20-UNSEL
                                     RECT-AT_200_200-VEL_23_-14-UNSEL
                                     false)
                         205 105 BUTTON-DOWN-EVENT)
= (make-world RECT-AT_200_100-VEL_-12_20-SEL_205_105
              RECT-AT_200_200-VEL_23_-14-UNSEL
              false)
|#

;; DESIGN STRATEGY: Use template for WorldState on w
(define (world-after-mouse-event w mx my mev)
  (make-world
    (rects-after-mouse-event (world-rects w) mx my mev)
    (world-paused? w)))


;; rects-after-mouse-event: ListOfRectangle Integer Integer MouseEvent -> ListOfRectangle 
;; GIVEN: A list of rectangles of some world state, the x- and y-coordinates of
;; a mouse event, and the mouse event
;; RETURNS: the list of rectangles where each rectangle should follow the rectangle
;; in the given list after the given mouse event
;; EXAMPLES:
#|
(rects-after-mouse-event RECTANGLE-LIST 205 105 BUTTON-DOWN-EVENT)
= (list RECT-AT_200_100-VEL_-12_20-SEL_205_105 RECT-AT_200_200-VEL_23_-14-UNSEL)
|#

;; DESIGN STRATEGY: Use template for ListOfRectangle on rects

(define (rects-after-mouse-event rects mx my mev)
  (cond
    [(empty? rects) empty]
    [else (cons
           (rect-after-mouse-event (first rects) mx my mev)
           (rects-after-mouse-event (rest rects) mx my mev))]))


;; rect-after-mouse-event: Rectangle Integer Integer MouseEvent -> Rectangle
;; GIVEN: A rectangle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the rectangle that should follow the given rectangle after
;; the given mouse event
;; EXAMPLES:
#|
(rect-after-mouse-event RECT-AT_200_100-VEL_-12_20-UNSEL 205 105 BUTTON-DOWN-EVENT)
= RECT-AT_200_100-VEL_-12_20-SEL_205_105

(rect-after-mouse-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 230 130 DRAG-EVENT)
= (make-rect 225 125 -12 20 true 230 130)

(rect-after-mouse-event RECT-AT_200_100-VEL_-12_20-SEL_205_105 BUTTON-UP-EVENT)
= RECT-AT_200_100-VEL_-12_20-UNSEL
|#

;; DESIGN STRATEGY: Cases on MouseEvent mev
(define (rect-after-mouse-event r mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN-EVENT) (rect-after-button-down r mx my)]
    [(mouse=? mev DRAG-EVENT) (rect-after-drag r mx my)]
    [(mouse=? mev BUTTON-UP-EVENT) (rect-after-button-up r)]
    [else r]))


;; rect-after-button-down: Rectangle Integer Integer -> Rectangle
;; GIVEN: A rectangle and the x- and y-coordinates of a mouse
;; RETURNS: the rectangle following a button-down at the given location.
;; EXAMPLES:
#|
(rect-after-button-down RECT-AT_200_100-VEL_-12_20-UNSEL 205 105)
= RECT-AT_200_100-VEL_-12_20-SEL_205_105
|#

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-button-down r mx my)
  (if (in-rect? r mx my)
      (make-rect (rect-x r)
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
#|
(rect-after-drag RECT-AT_200_100-VEL_-12_20-SEL_205_105 230 130)
= (make-rect 225 125 -12 20 true 230 130)
|#

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-drag r mx my)
  (if (rect-selected? r)
      (make-rect (+ (rect-x r) (- mx (rect-mx r)))
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
#|
(rect-after-button-up RECT-AT_200_100-VEL_-12_20-SEL_205_105)
= RECT-AT_200_100-VEL_-12_20-UNSEL
|#

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (rect-after-button-up r)
  (if (rect-selected? r)
      (make-rect (rect-x r)
                 (rect-y r)
                 (rect-vx r)
                 (rect-vy r)
                 false
                 DEFAULT-MX
                 DEFAULT-MY)
      r))


;; TESTS:

(begin-for-test
  (check-equal?
   (world-after-mouse-event 
    (make-world (list RECT-AT_200_100-VEL_-12_20-UNSEL
                      RECT-AT_200_200-VEL_23_-14-UNSEL)
                false)
    205 105 BUTTON-DOWN-EVENT)
   (make-world (list RECT-AT_200_100-VEL_-12_20-SEL_205_105
                     RECT-AT_200_200-VEL_23_-14-UNSEL)
               false)
   "Button down inside rectangle should select that rectangle only.")
  
  (check-equal?
   (world-after-mouse-event 
    (make-world (list RECT-AT_200_100-VEL_-12_20-SEL_205_105
                      RECT-AT_200_200-VEL_23_-14-UNSEL)
                false)
    230 130 DRAG-EVENT)
   (make-world (list (make-rect 225 125 -12 20 true 230 130)
                     RECT-AT_200_200-VEL_23_-14-UNSEL)
               false)
   "Drag should move selected rectangles only.")
  
  (check-equal?
   (world-after-mouse-event
    (make-world (list RECT-AT_200_100-VEL_-12_20-SEL_205_105
                      RECT-AT_200_200-VEL_23_-14-UNSEL)
                true)
    205 105 BUTTON-UP-EVENT)
   (make-world (list RECT-AT_200_100-VEL_-12_20-UNSEL
                     RECT-AT_200_200-VEL_23_-14-UNSEL)
               true)
   "button-up should unselect the selected rectangles.")
  
  (check-equal?
   (world-after-mouse-event
    UNPAUSED-WORLD-UNSEL-RECT 
    50 50 MOVE-EVENT)
   UNPAUSED-WORLD-UNSEL-RECT
   "other mouse events should leave the world unchanged."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; in-rect?: Rectangle Integer Integer -> Rectangle
;; GIVEN: the rectangle r and x & y co-ordinates of mouse
;; RETURNS: true if the given coordinate is inside the given rectangle.
;; EXAMPLES:
;; (in-rect? RECT-AT_200_100-VEL_-12_20-UNSEL 205 105) = true
;; (in-rect? RECT-AT_200_100-VEL_-12_20-UNSEL 250 105) = false

;; DESIGN STRATEGY: Use template for Rectangle on r
(define (in-rect? r mx my)
  (and (<= (- (rect-x r) HALF-RECTANGLE-WIDTH)
           mx
           (+ (rect-x r) HALF-RECTANGLE-WIDTH))
       (<= (- (rect-y r) HALF-RECTANGLE-HEIGHT)
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