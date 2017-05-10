#lang racket
;; Target.rkt : Class definition for Target class.

;; Require statements:
(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

;; Provide
(provide Target%
         make-target)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; A Target is a
;; (new Target% [x Integer] [y Integer] [selected? Boolean] [mx Integer] [my Integer])
;; A Target represents a target in the playground which is used to interact with the toys.

(define Target%
  (class* object% (Target<%>)
    
    ;; the init-fields are the values that may vary from one Target to the next.
    
    ;; the x and y position of the center of the Target
    (init-field x y)   
    
    ; is the Target selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the Target is selected, the position of the last button-down event inside
    ;; the Target, relative to the Target's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    
    ;; private data for objects of this class, these can depend on the init-fields.
    
    ; the Target's radius
    (field [r 10])   
    
    ; image for displaying the unselected Target
    (field [TARGET-IMG (circle r "outline" "blue")])
    
    ; image for displaying the selected Target
    (field [SEL-TARGET-IMG (circle r "outline" "red")])
    
    (super-new)
    
    ;; target-x: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: the x coordinate of center of target
    ;; EXAMPLE: a Target with center at (10,14) will return 10 for target-x function
    (define/public (target-x) x)
    
    ;; target-y: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: the x coordinate of center of target
    ;; EXAMPLE: a Target with center at (10,14) will return 14 for target-y function.
    (define/public (target-y) y)
    
    ;; target-selected?: -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true iff the target is selected?, otherwise false
    (define/public (target-selected?) selected?)
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: no changes in the state of the target after each tick.
    (define/public (after-tick) this)
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: no changes in the state of the target after key event.
    (define/public (after-key-event kev) this)      
    
    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this target to the state it should have following the button down 
    ;;         mouse event at the given location.
    ;; EXAMPLE: Consider a target t1 is present in playground. Once a mouse button
    ;;         down event occurs, and the mouse pointer is inside the circle then
    ;;         target gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the Target
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))) 
          this))
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this target to the state it should have following the button up
    ;;         mouse event at the given location.
    ;; EXAMPLE: target gets unselected after button up event.
    ;; STRATEGY: Combine Simpler functions
    (define/public (after-button-up mx my) 
      (set! selected? false))   
    
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this target to the state it should have following the drag mouse 
    ;;         event at the given location.
    ;; EXAMPLE: If target is selected, moves it so that the vector from the center to
    ;;          the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether the Target is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin (set! x (- mx saved-mx))
                 (set! y (- my saved-my))
                 (set! selected? true))
          this))  
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a Scene like the given one, but with this Target painted on it.
    ;; EXAMPLE: Suppose we have an empty canvas, the function will paint a outline circle
    ;;          image on it.
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (if selected? SEL-TARGET-IMG TARGET-IMG) 
                   x
                   y
                   scene))
    
    ;; in-target? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this target. Determined using the formula
    ;;          for points inside a circle (including the edge):
    ;;          (x - other-x)^2 + (y - other-y)^2 <= r^2
    ;; STRATEGY: Combine simpler functions
    (define (in-target? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
    ))

;; make-target : -> Target<%>
;; GIVEN: no arguments
;; RETURNS: a new target object placed at it's intital x,y position.
(define (make-target)
  (new Target% [x HALF-CANVAS-WIDTH][y HALF-CANVAS-HEIGHT]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS FOR TESTING:

(define TARGET-RADIUS 10)
(define TARGET-MODE "outline")
(define TARGET-COLOR "blue")
(define SEL-TARGET-COLOR "red")
(define NEW-SQUARE-EVENT "s")
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define t0 (make-target))) 
    
    (send t0 after-tick)
    (check-equal?
     (send t0 target-x)
     HALF-CANVAS-WIDTH
     "Target after tick.")
    
    (send t0 after-key-event NEW-SQUARE-EVENT)
    (check-equal?
     (send t0 target-y)
     HALF-CANVAS-HEIGHT
     "Target after key event.")
    
    (check-equal?
     (send t0 add-to-scene EMPTY-CANVAS)
     (place-image (circle TARGET-RADIUS TARGET-MODE TARGET-COLOR)
                  HALF-CANVAS-WIDTH
                  HALF-CANVAS-HEIGHT
                  EMPTY-CANVAS)
     "Unselected Target to scene")
    
    (send t0 after-button-down HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send t0 target-selected?)
     "Target mouse button down inside")
    
    (check-equal?
     (send t0 add-to-scene EMPTY-CANVAS)
     (place-image (circle TARGET-RADIUS TARGET-MODE SEL-TARGET-COLOR)
                  HALF-CANVAS-WIDTH
                  HALF-CANVAS-HEIGHT
                  EMPTY-CANVAS)
     "Selecetd Target to scene")
    
    (send t0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send t0 target-selected?)
     "Selected Target mouse drag")
    
    (send t0 after-button-down (add1 (+ HALF-CANVAS-WIDTH TARGET-RADIUS))
          HALF-CANVAS-HEIGHT)
    (check-true
     (send t0 target-selected?)
     "Target mouse button down outside")
    
    (send t0 after-button-up HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-false
     (send t0 target-selected?)
     "Target mouse button up")
    
    (send t0 after-drag (add1 (+ HALF-CANVAS-WIDTH TARGET-RADIUS)) HALF-CANVAS-HEIGHT)
    (check-false
     (send t0 target-selected?)
     "Unselected Target mouse drag")))