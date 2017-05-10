#lang racket
;; Football.rkt : Class definition for Football class.

;; Require statements:
(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

;; Provide
(provide Football%
         make-football)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; A Football is a
;; (new Football% [x Integer] [y Integer] [size Number] [selected? Boolean]
;;                [mx Integer] [my Integer])
;; A Football represents a football toy in the playground.


(define Football%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one Football to
    ;; the next.
    
    ;; the x and y position of the center of the Football
    (init-field x y)   
    
    ;; a Parameter representing size of football. Bigger the number larger the size
    (init-field [size 1]) 
    
    ;; is the Football selected? Default is false. 
    (init-field [selected? false])
    
    ;; if the Football is selected, the position of the last button-down event inside
    ;; the Football, relative to the Football's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; The image of football
    (field [FOOTBALL-IMG (bitmap "football.jpg")])
    
    ;; Minimum size of the football after widget will not be visible
    (field [MIN-SCALE-SIZE 0.1])
    
    ;; Size unit for returning the football size
    (field [SIZE-UNIT 10])
    
    ;; half the image width of football after scaling
    (field [HALF-IMG-WIDTH  (if (>= size MIN-SCALE-SIZE)
                                (/ (image-width (scale size FOOTBALL-IMG)) 2)
                                0)])
    
    ;; half the image height of football after scaling
    (field [HALF-IMG-HEIGHT (if (>= size MIN-SCALE-SIZE)
                                (/ (image-height (scale size FOOTBALL-IMG)) 2)
                                0)])
    
    (super-new)

    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this football to the state it should have following the tick.
    ;; EXAMPLE: the function updates the football to the state that has shrinked by
    ;;          another 10% than original. If the football reaches a size less than 10%,
    ;;          from next tick it is not shrinked. So if we have a football enclosed in
    ;;          square of side 100 at next tick we will have football of side 90.
    ;; STRATEGY: Cases on selected? and value of size
    (define/public (after-tick)
      (if (or selected? (< size MIN-SCALE-SIZE))
          this
          (begin (set! size (- size MIN-SCALE-SIZE)))))
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: no changes in the state of the football after key event.
    (define/public (after-key-event kev) this)      
    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this football to the state it should have following the button
    ;;         down  mouse event at the given location.
    ;; EXAMPLE: Consider a football f1 is present in playground. Once a mouse button
    ;;         down event occurs, and the mouse pointer is inside the football then
    ;;         football gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the Football
    (define/public (after-button-down mx my)
      (if (and (in-football? mx my)
               (>= size MIN-SCALE-SIZE))
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y))) 
          this))
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this football to the state it should have following the button up
    ;;         mouse event at the given location.
    ;; EXAMPLE: football gets unselected after button up event.
    ;; STRATEGY: Combine Simpler functions
    (define/public (after-button-up mx my)
      (set! selected? false))
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this football to the state it should have following the drag mouse 
    ;;         event at the given location.
    ;; EXAMPLE: If football is selected, moves it so that the vector from the center to
    ;;          the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether the Football is selected or not
    (define/public (after-drag mx my)
      (if (and selected? (>= size MIN-SCALE-SIZE))
          (begin (set! x (- mx saved-mx))
                 (set! y (- my saved-my))
                 (set! selected? true))
          this))  
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this football painted on it if it
    ;;          is above size limit.
    ;; EXAMPLE: Suppose we have an empty canvas the function will paint a football image
    ;;          scaled down as per the current size of football.If the size is below limit
    ;;          then football is not painted on it.
    ;; STRATEGY: Cases on size
    (define/public (add-to-scene scene)
      (if (>= size MIN-SCALE-SIZE)
          (place-image (scale size FOOTBALL-IMG) x y scene)
          scene))
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    (define/public (toy-x) x)
    
    (define/public (toy-y) y)
    
    ;; -> Int
    ;; RETURNS: the current size of the football (in
    ;; arbitrary units; bigger is more)
    (define/public (toy-data) (* SIZE-UNIT size))
    
    ;; -> Boolean
    ;; Returns: whether football is selected
    (define/public (for-test:selected?) selected?)
    
    ;; in-football? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this football.
    ;; STRATEGY: Combine simpler functions
    (define (in-football? other-x other-y)
      (and (<= (- x HALF-IMG-WIDTH) other-x (+ x HALF-IMG-WIDTH))
           (<= (- y HALF-IMG-HEIGHT) other-y (+ y HALF-IMG-HEIGHT))))
    ))

;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
(define (make-football x y)
  (new Football% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS FOR TESTING:
(define SIZE-UNIT 10)
(define NEW-SQUARE-EVENT "s")
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define f0 (make-football HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)))

    (send f0 after-tick)
    (check-equal?
     (send f0 toy-x)
     HALF-CANVAS-WIDTH
     "Unselected Football after tick")

    (send f0 after-key-event NEW-SQUARE-EVENT)
    (check-equal?
     (send f0 toy-y)
     HALF-CANVAS-HEIGHT
     "Unselected Football after key event")

    (send f0 after-button-down (+ HALF-CANVAS-WIDTH CANVAS-WIDTH) HALF-CANVAS-HEIGHT)
    (check-false
     (send f0 for-test:selected?)
     "Football mouse button down outside")

    (send f0 after-button-up (+ HALF-CANVAS-WIDTH CANVAS-WIDTH) HALF-CANVAS-HEIGHT)
    (check-false
     (send f0 for-test:selected?)
     "Football mouse button up outside")

    (send f0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-false
     (send f0 for-test:selected?)
     "Selected Football mouse drag")

    (send f0 after-button-down HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send f0 for-test:selected?)
     "Football mouse button down inside")

    (send f0 after-tick)
    (check-equal?
     (send f0 toy-data)
     9.0
     "Selected Football after tick")
    
    (send f0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send f0 for-test:selected?)
     "Selected Football mouse drag")
    
    (check-equal?
     (send (make-football HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT) add-to-scene EMPTY-CANVAS)
     (place-image (bitmap "football.jpg")
                  HALF-CANVAS-WIDTH
                  HALF-CANVAS-HEIGHT
                  EMPTY-CANVAS)
     "Football to scene")

    (check-equal?
     (send (new Football% [x HALF-CANVAS-WIDTH] [y HALF-CANVAS-HEIGHT] [size 0])
           add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS
     "Football to scene with size 0")))