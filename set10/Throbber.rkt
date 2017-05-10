#lang racket
;; Throbber.rkt : Class definition for Throbber class.

;; Require statements:
(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

;; Provide
(provide Throbber%
         make-throbber)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; A Throbber is a
;; (new Throbber% [x Integer] [y Integer] [r Integer] [rate Integer] [selected? Boolean]
;;                [mx Integer] [my Integer])
;; A Throbber represents a throbber toy in the playground.

(define Throbber%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one Throbber to the next.
    
    ; the x and y position of the center of the Throbber
    (init-field x y)
    
    ; is the Throbber selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the Throbber is selected, the position of the last button-down event inside
    ;; the Throbber, relative to the Throbber's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; the Throbber's radius, Default is 5.
    (init-field [r 5])
    
    ;; the rate at which throbber expands/contracts, Default is 1.
    (init-field [rate 1])
    
    ;; private data for objects of this class. These can depend on the init-fields.
    
    ;; the minimum radius to which a throbber shrinks
    (field [MIN-R 5])
    
    ;; the maximum radius to which a throbber expands
    (field [MAX-R 20])
    
    ;; circle properties
    (field [CIRCLE-MODE "solid"])
    (field [SEL-CIRCLE-COLOR "red"])
    (field [CIRCLE-COLOR "green"])
    
    (super-new)
    
    ;; toy-x: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the x position of the center of throbber
    ;; EXAMPLE: a Throbber with center at (10,14) will return 10 for toy-x function.
    (define/public (toy-x) x)
    
    ;; toy-y: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the y position of the center of throbber
    ;; EXAMPLE: a Throbber with center at (10,14) will return 14 for toy-y function.
    (define/public (toy-y) y)
    
    ;; toy-data: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the radius of the throbber
    (define/public (toy-data) r)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: Updates this throbber to the state it should have following the tick.
    ;; EXAMPLE: the function updates the throbber to the state where it has expanded or
    ;;          shrinked by throbber's expansion/contraction rate. Once it reaches to
    ;;          its minimum or maximum radius, rate is reversed. Selected throbber will
    ;;          remain in the same state after tick.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (begin (set! r (+ r rate))
                 (set! rate (rate-after-tick)))))
    
    ;; rate-after-tick : -> Integer 
    ;; GIVEN: no arguments
    ;; RETURNS: this throbber's expansion/contraction rate.
    ;; EXAMPLE: If the radius reaches to its minimum or maximum value then the rate is
    ;;          reversed.
    ;; STRATEGY: Divide into cases on value of radius
    (define (rate-after-tick)
      (if (or (= r MIN-R) (= r MAX-R))
          (- rate)
          rate))

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a KeyEvent kev
    ;; EFFECT: No change in the state of this throbber.
    (define/public (after-key-event kev) this)      
    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this throbber to the state it should have following the button down 
    ;;         mouse event at the given location.
    ;; EXAMPLE: Consider a throbber t1 is present in playground. Once a mouse button
    ;;         down event occurs, and the mouse pointer is inside the circle then
    ;;         throbber gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the Throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))) 
          this))
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this throbber to the state it should have following the button up
    ;;         mouse event at the given location.
    ;; EXAMPLE: throbber gets unselected after button up event.
    ;; STRATEGY: Combine Simpler functions
    (define/public (after-button-up mx my)
      (set! selected? false)) 
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this throbber to the state it should have following the drag mouse 
    ;;         event at the given location.
    ;; EXAMPLE: If throbber is selected, moves it so that the vector from the center to
    ;;          the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether the Throbber is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin (set! x (- mx saved-mx))
                 (set! y (- my saved-my))
                 (set! selected? true))
          this))   
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this throbber painted on it.
    ;; EXAMPLE: Suppose we have an empty canvas then function will paint a solid green
    ;;          circle of initial radius on canvas.          
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (circle r CIRCLE-MODE (if selected? SEL-CIRCLE-COLOR CIRCLE-COLOR))
                   x
                   y
                   scene))
    
    ;; test methods, to probe the throbber state.  
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)
    
    ;; in-throbber? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this throbber. Determined using the
    ;;          formula for points inside a circle (including the edge):
    ;;          (x - other-x)^2 + (y - other-y)^2 <= r^2
    ;; STRATEGY: Combine simpler functions
    (define (in-throbber? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
    ))


;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
(define (make-throbber x y)
  (new Throbber% [x x][y y]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS FOR TESTING:

(define THROBBER-RADIUS 5)
(define THROBBER-MIN-RADIUS 5)
(define THROBBER-MAX-RADIUS 20)
(define THROBBER-MODE "solid")
(define THROBBER-COLOR "green")
(define SEL-THROBBER-COLOR "red")
(define THROBBER-EXP-CON-RATE 1)
(define NEW-SQUARE-EVENT "s")
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define t0 (make-throbber HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT))
     (define t1 (new Throbber%
                     [x (send t0 toy-x)]
                     [y (send t0 toy-y)]
                     [r (sub1 THROBBER-MAX-RADIUS)]))) 

    (send t0 after-key-event NEW-SQUARE-EVENT)
    (check-equal?
     (send t0 toy-data)
     5
     "Throbber after key event.")
    
    (send t0 after-button-down HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-equal?
     (send t0 add-to-scene EMPTY-CANVAS)
     (place-image (circle THROBBER-RADIUS THROBBER-MODE SEL-THROBBER-COLOR)
                  HALF-CANVAS-WIDTH
                  HALF-CANVAS-HEIGHT
                  EMPTY-CANVAS)
     "Selecetd Throbber to scene")
    
    (check-true
     (send t0 for-test:selected?)
     "Throbber mouse button down inside")
    
    
    (send t0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send t0 for-test:selected?)
     "Selected Throbber mouse drag")
    
    (send t0 after-tick)
    (send t0 after-button-down (+ HALF-CANVAS-WIDTH 100) HALF-CANVAS-HEIGHT)
    (check-true
     (send t0 for-test:selected?)
     "Throbber mouse button down outside")
    
    (send t0 after-button-up HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-false
     (send t0 for-test:selected?)
     "Throbber mouse button up inside")
    
    (check-equal?
     (send t0 add-to-scene EMPTY-CANVAS)
     (place-image (circle THROBBER-RADIUS THROBBER-MODE THROBBER-COLOR)
                  HALF-CANVAS-WIDTH
                  HALF-CANVAS-HEIGHT
                  EMPTY-CANVAS)
     "Unselected Throbber to scene")
    
    (send t0 after-drag (+ HALF-CANVAS-WIDTH 100) HALF-CANVAS-HEIGHT)
    (check-false
     (send t0 for-test:selected?)
     "Unselected Throbber mouse drag")

    (check-equal?
     (send t0 toy-data)
     5
     "Unselected Throbber after one tick.")

    (send t1 after-tick)
    (check-equal?
     (send t1 toy-data)
     THROBBER-MAX-RADIUS
     "Unselected Throbber after one tick reaches to maximum radius.")

    (send t1 after-tick)
    (check-equal?
     (send t1 toy-data)
     (sub1 THROBBER-MAX-RADIUS)
     "Unselected Throbber after one tick reaches to maximum radius.")))