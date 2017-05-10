#lang racket
;; Square.rkt : Class definition for Square class.

;; Require statements:
(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

;; Provide
(provide Square%
         make-square-toy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; The Square% class
;; A Square is a
;; (new Square% [x Integer] [y Integer] [velocity Integer] [selected? Boolean]
;;              [ mx Integer] [my Integer])
;; A Square represents a square toy in the playground.
(define Square%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one square to the next.
    
    ;; the x and y position of the center of the square
    (init-field x y)
    
    ;; the velocity of square along x-axis 
    (init-field velocity)
    
    ;; is the square selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the square is selected, the position of the last button-down event inside
    ;; the square, relative to the sqaure's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ;; the square's radius
    (field [SIDE 40])
    
    ;; half the length of sqaure's side
    (field [HALF-SIDE (/ SIDE 2)])
    
    ;; the leftmost position on canvas of square's center during it's motion without drag
    (field [MIN-X HALF-SIDE])
    
    ;; the rightmost position on canvas of square's center during it's motion without drag
    (field [MAX-X (- CANVAS-WIDTH HALF-SIDE)])
    
    ;; the topmost position on canvas of square's center during it's motion without drag
    (field [MIN-Y HALF-SIDE])
    
    ;; the bottommost position on canvas of square's center during it's motion
    ;;  without drag
    (field [MAX-Y (- CANVAS-HEIGHT HALF-SIDE)])
    
    ;; image for displaying unselected square
    (field [SQUARE-IMG (square SIDE "outline" "blue")])
    
    ;; image for displaying selected square
    (field [SEL-SQUARE-IMG (square SIDE "outline" "red")])
    
    (super-new)
    
    ;; toy-x: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the x position of the center of square
    ;; EXAMPLE: a Square with center at (10,14) will return 10 for toy-x function
    (define/public (toy-x) x)
    
    ;; toy-y: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the y position of the center of square
    ;; EXAMPLE: a Square with center at (10,14) will return 14 for toy-y function
    (define/public (toy-y) y)
    
    ;; toy-data: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the velocity of the square (rightward is positive)
    (define/public (toy-data) velocity)
    
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this square to the state it should have following a tick.
    ;; EXAMPLE: the function updates the state of this square to the state after moving
    ;;          horizontally as per its velocity. When its edge reaches the edge of the
    ;;          canvas, it executes a Perfect Bounce.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (let ((x1 (square-x-after-tick))
                (v1 (velocity-after-tick)))
            (begin
              (set! x x1)
              (set! velocity v1)
              (set! y (square-y-after-tick))))))
    
    ;; square-x-after-tick: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Value of x-coordinate of center of this square after a tick
    ;; EXAMPLE: if square's edge touches or go past the right side of canvas it
    ;;          returns max-x, if square's edge touches or go past the left side
    ;;          of canvas it returns min-x else it returns the sum of its current
    ;;          x-coordinate and velocity
    ;; STRATEGY: Combine simpler functions
    (define (square-x-after-tick)
      (limit-value MIN-X (+ x velocity) MAX-X))
    
    ;; square-y-after-tick: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Value of y-coordinate of center of this square after a tick
    ;; EXAMPLE: if square's edge touches or go past the bottom side of canvas it
    ;;          returns max-y, if square's edge touches or go past the top side
    ;;          of canvas it returns min-y else it returns the its current y-coordinate
    ;; STRATEGY: Combine simpler functions
    (define (square-y-after-tick)
      (limit-value MIN-Y y MAX-Y))
    
    ;; velocity-after-tick: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Value of velocity of this square after a tick
    ;; EXAMPLE: if square's edge touches or go past the right or left side of canvas it
    ;;          returns the negative of current velocity else returns the same velocity
    ;; STRATEGY: Divide into cases on whether square has passed left or right border of
    ;;           canvas
    (define (velocity-after-tick)
      (if (or (<= (+ x velocity) MIN-X)
              (>= (+ x velocity) MAX-X))
          (- velocity)
          velocity))
    
    ;; limit-value : Number^3 -> Number
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define (limit-value lo val hi)
      (max lo (min val hi)))
    

    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: no changes in the state of the square after key event.
    (define/public (after-key-event kev) this)      

    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this square to the state it should have following the button down 
    ;;         mouse event at the given location.
    ;; EXAMPLE: Consider a square s1 is present in playground. Once a mouse button
    ;;         down event occurs, and the mouse pointer is inside the square then
    ;;         square gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the Square
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))  
          this))
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this square to the state it should have following the button up
    ;;         mouse event at the given location.
    ;; EXAMPLE: square gets unselected after button up event.
    ;; STRATEGY: Combine Simpler functions
    (define/public (after-button-up mx my)
      (set! selected? false))  
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this square to the state it should have following the drag mouse 
    ;;         event at the given location.
    ;; EXAMPLE: If square is selected, moves it so that the vector from the center to
    ;;          the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether the Square is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin (set! x (- mx saved-mx))
                 (set! y (- my saved-my))
                 (set! selected? true))
          this))  
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this square painted on it.
    ;; EXAMPLE: Suppose we have an empty canvas the function will paint a square image
    ;;          on it centered at x,y co-ordinate.
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (if selected? SEL-SQUARE-IMG SQUARE-IMG)
                   x
                   y
                   scene))
    
    ;; test methods, to probe the square state.  
    ;; -> Boolean
    (define/public (for-test:selected?)
      selected?)
    
    ;; in-square?: Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this square.
    ;; STRATEGY: Combine simpler functions
    (define (in-square? other-x other-y)
      (and (<= (- x HALF-SIDE) other-x (+ x HALF-SIDE))
           (<= (- y HALF-SIDE) other-y (+ y HALF-SIDE))))
    ))


;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
(define (make-square-toy x y v)
  (new Square% [x x][y y][velocity v]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS FOR TESTING:

(define SQUARE-SIDE 40)
(define HALF-SQUARE-SIDE (/ SQUARE-SIDE 2))
(define SQUARE-SPEED 5)
(define SQUARE-MODE "outline")
(define SQUARE-COLOR "blue")
(define SEL-SQUARE-COLOR "red")
(define NEW-SQUARE-EVENT "s")
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define s0 (make-square-toy HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT SQUARE-SPEED))
     (define s1 (new Square% [x CANVAS-WIDTH] [y CANVAS-HEIGHT] [velocity SQUARE-SPEED]))
     (define s2 (new Square% [x 0] [y 0] [velocity (- SQUARE-SPEED)]))
     (define s3 (new Square% [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT]
                     [velocity SQUARE-SPEED]))
     (define s4 (make-square-toy HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT SQUARE-SPEED)))
    
    (send s0 after-tick)
    (check-equal?
     (send s0 toy-x)
     (+ HALF-CANVAS-WIDTH SQUARE-SPEED)
     "Unselected Square inside the canvas moves right after tick")
    
    (send s1 after-tick)
    (check-equal?
     (send s1 toy-x)
     (- CANVAS-WIDTH HALF-SQUARE-SIDE)
     "Unselected Square outside the right-bottom corner after tick")
    
    (send s2 after-tick)
    (check-equal?
     (send s2 toy-x)
     HALF-SQUARE-SIDE
     "Unselected Square outside the top-left corner after tick")
    
    (send s3 after-tick)
    (check-equal?
     (send s3 toy-x)
     255
     "Selected Square x after tick")
    
    (check-equal?
     (send s3 toy-y)
     HALF-CANVAS-HEIGHT
     "Selected Square y after tick")
    
    (check-equal?
     (send s0 toy-data)
     SQUARE-SPEED
     "Selected Square velocity after tick")
    
    (send s0 after-key-event NEW-SQUARE-EVENT)
    (check-equal?
     (send s0 toy-x)
     (send s0 toy-x)
     "Square after key event.")
    
    (send s0 after-button-down HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send s0 for-test:selected?)
     "Square mouse button down inside")

    (send s0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send s0 for-test:selected?)
     "Selected Square mouse drag")
    
    (send s0 after-button-down (+ HALF-CANVAS-WIDTH SQUARE-SIDE) HALF-CANVAS-HEIGHT)
    (check-true
     (send s0 for-test:selected?)
     "Square mouse button down outside")
    
    (send s0 after-button-up HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-false
     (send s0 for-test:selected?)
     "Square mouse button up inside")
        
    (send s0 after-drag (+ HALF-CANVAS-WIDTH SQUARE-SIDE) HALF-CANVAS-HEIGHT)
    (check-false
     (send s0 for-test:selected?)
     "Unselected Square mouse drag")
    
    (check-equal?
     (send s4 add-to-scene EMPTY-CANVAS)
     (place-image (square SQUARE-SIDE SQUARE-MODE SQUARE-COLOR)
                  HALF-CANVAS-WIDTH
                  HALF-CANVAS-HEIGHT
                  EMPTY-CANVAS)
     "Unselected Square to scene")
    
    (send s4 after-button-down HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (send s4 after-tick)
    
    (check-equal?
     (send s4 add-to-scene EMPTY-CANVAS)
     (place-image (square SQUARE-SIDE SQUARE-MODE SEL-SQUARE-COLOR)
                  HALF-CANVAS-WIDTH
                  HALF-CANVAS-HEIGHT
                  EMPTY-CANVAS)
     "Selecetd Square to scene")))