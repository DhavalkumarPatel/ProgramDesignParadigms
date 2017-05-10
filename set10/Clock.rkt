#lang racket
;; Clock.rkt : Class definition for Clock class.

;; Require statements:
(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

;; Provide
(provide Clock%
         make-clock)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; A Clock is a
;; (new Clock% [x Integer] [y Integer] [timer Integer] [selected? Boolean]
;;             [mx Integer] [my Integer])
;; A Clock represents a clock toy in the playground.

(define Clock%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one Clock to the next.
    
    ;; the x and y position of the center of the Clock
    (init-field x y)   
    
    ;; the timer represents time in ticks since clock was created
    (init-field [timer 0]) 
    
    ;; is the Clock selected? Default is false. 
    (init-field [selected? false])
    
    ;; if the clock is selected, the position of the last button-down event inside the
    ;; Clock, relative to the Clock's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; Radius of clock
    (field [r 60])
    
    ;; Radius in which clock's hand moves
    (field [CLOCK-HAND-RADIUS 40])
    
    ;; Color of Clock's hand
    (field [CLOCK-HAND-COLOR "red"])
    
    ;; Digits to be displayed in clock
    (field [DIGITS-IN-CLOCK (list 270 180 90 0)])
    
    ;; Size of digits
    (field [DIAL-TEXT-SIZE 12])
    
    ;; size of timer text
    (field [TEXT-SIZE 15])
    
    ;; Color of dial digits
    (field [DIAL-TEXT-COLOR "black"])
    
    ;; Color of timer text
    (field [TIMER-TEXT-COLOR "black"])
    
    ;; Circle bounding the overall clock
    (field [CLOCK-CIRCLE (circle r "outline" "black")])
    
    ;; Count of degrees present in a circle
    (field [DEGREE-IN-CIRCLE 360])
    
    ;; Number of degrees present in a half circle 
    (field [HALF-DEGREE-IN-CIRCLE 180])
    
    ;; Number of degrees present in a quarter circle
    (field [QUARTER-DEGREE-IN-CIRCLE 90])
    
    ;; Positioning of a digit along y axis inside a circle
    (field [CENTER "center"])
    
    ;; Positioning of a digit along x axis inside a circle
    (field [TOP "top"])
    
    (super-new)
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: Updates this clock to the state it should have following the tick.
    ;; EXAMPLE: Consider a clock c0 which has been created since 109 ticks. At next tick
    ;;          the function updates the timer of this clock to 110. 
    ;; STRATEGY: Combine simpler functions
    (define/public (after-tick)
      (set! timer (add1 timer)))
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: No changes in the state of the clock after key event.
    (define/public (after-key-event kev) this)      
    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: Updates this clock to the state it should have following the button down 
    ;;         mouse event at the given location.
    ;; EXAMPLE: Consider a clock c1 is present in playground. Once a mouse button
    ;;         down event occurs, and the mouse pointer is inside the circle then
    ;;         clock gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the Clock
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))            
          this))
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this clock to the state it should have following the button up
    ;;         mouse event at the given location.
    ;; EXAMPLE: clock gets unselected after button up event.
    ;; STRATEGY: Combine Simpler functions
    (define/public (after-button-up mx my)
      (set! selected? false))  
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this clock to the state it should have following the drag mouse 
    ;;         event at the given location.
    ;; EXAMPLE: If clock is selected, moves it so that the vector from the center to
    ;;          the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether the Clock is selected or not
    (define/public (after-drag mx my)
      (if selected?
          (begin (set! x (- mx saved-mx))
                 (set! y (- my saved-my))
                 (set! selected? true))
          this))   
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this clock painted on it.
    ;; EXAMPLE: If we are provided an empty canvas function will paint a clock with
    ;;          digits 0 ,90 ,180, 270 as counters and a red clock-hand with the number
    ;;          of ticks since its creation displayed at center of clock.
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image
       (text (number->string timer) TEXT-SIZE TIMER-TEXT-COLOR) x y
       (draw-clock-hand scene)))
    
    ;; draw-clock-hand: Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this clock dial and hand painted
    ;; on it.
    ;; EXAMPLE: Given an empty canvas it will draw a circular clock and an clock hand as
    ;;          a line pointing to 0. 
    ;; STRATEGY: Combine simpler functions
    (define (draw-clock-hand scene)
      (scene+line
       (place-image (place-all-numbers CLOCK-CIRCLE) x y scene) x y
       (new-hand-x-pos)
       (new-hand-y-pos)
       CLOCK-HAND-COLOR))
    
    ;; new-hand-x-pos: -> Real
    ;; GIVEN : no arguments
    ;; RETURNS: the x-coordinate of clock-hand
    ;; EXAMPLE: for a clock created one second before ,it will provide
    ;;          the x-coordinate of a point which is moved by one degree
    ;;          along circumference of a circle  with radius as length of clock's hand.
    ;; STRATEGY: Combine simpler functions
    (define (new-hand-x-pos)
      (+ x (* CLOCK-HAND-RADIUS
              (cos (degree-to-radian (- (modulo timer DEGREE-IN-CIRCLE)
                                        QUARTER-DEGREE-IN-CIRCLE))))))
    
    ;; new-hand-y-pos: -> Real
    ;; GIVEN : no arguments
    ;; RETURNS: the y-coordinate of clock-hand
    ;; EXAMPLE: for a clock created one second before ,it will provide
    ;;          the y-coordinate of a point which is moved by one degree
    ;;          along circumference of a circle  with radius as length of clock's hand.
    ;; STRATEGY: Combine simpler functions
    (define (new-hand-y-pos)
      (+ y (* CLOCK-HAND-RADIUS
              (sin (degree-to-radian (- (modulo timer DEGREE-IN-CIRCLE)
                                        QUARTER-DEGREE-IN-CIRCLE))))))
    
    ;; degree-to-radian: -> Real
    ;; GIVEN : no arguments
    ;; RETURNS: the radian equivalent of given degrees
    ;; STRATEGY: Combine simpler functions
    ;; EXAMPLE: 360 degrees is equivalent to 2 pi
    (define (degree-to-radian degree)
      (* pi (/ degree HALF-DEGREE-IN-CIRCLE)))
    
    ;; place-all-numbers: Image -> Image
    ;; GIVEN : Image of clock
    ;; RETURNS: places all digits inside the clock
    ;; EXAMPLE: Given an image of a circle it will place digits in digits list, (here
    ;;          0,90,180,270) by rotating 90 degrees after placing each digit.
    ;; STRATEGY: Use HOF foldr on DIGITS-IN-CLOCK
    (define (place-all-numbers dial)
      (foldr
       ;; Integer Image-> Image
       ;; GIVEN : the number to be represented, and image of clock
       ;; RETURNS: places given digit inside the clock
       (lambda (elt d)(place-and-turn elt  d))
       dial
       DIGITS-IN-CLOCK ))
    
    ;; place-and-turn: Integer Image-> Image
    ;; GIVEN : the number to be represented, and current image of clock
    ;; RETURNS: places given digit inside the clock
    ;; EXAMPLE: Places given digit, say 0 at center top position of given image say a
    ;;          circle
    ;; STRATEGY: Combine simpler functions
    (define (place-and-turn digit dial)
      (rotate QUARTER-DEGREE-IN-CIRCLE
              (overlay/align CENTER TOP
                             (a-number digit)
                             dial)))
    
    ;; a-number: Integer -> Image
    ;; GIVEN : the number to be represented
    ;; RETURNS: generates text iamge ofgiven integer
    ;; STRATEGY: Combine simpler functions
    (define (a-number digit)      
      (text (number->string digit) DIAL-TEXT-SIZE DIAL-TEXT-COLOR))
    
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the clock
    (define/public (toy-x) x)
    
    (define/public (toy-y) y)
    
    ;; -> Int
    ;; RETURNS: the current time in clock(ticks since it was created)
    (define/public (toy-data) timer)
    
    ;; -> Boolean
    ;; Returns: whether clock is selected
    (define/public (for-test:selected?)
      selected?)
    
    ;; in-clock? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this clock. Determined using the
    ;;          formula for points inside a circle (including the edge):
    ;;          (x - other-x)^2 + (y - other-y)^2 <= r^2
    ;; STRATEGY: Combine simpler functions
    (define (in-clock? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
    ))


;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
(define (make-clock x y)
  (new Clock% [x x][y y]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS FOR TESTING:

(define CLOCK-RADIUS 60)
(define HAND-RADIUS 40)
(define DIAL-TEXT 12)
(define DIGITS-LIST (list 270 180 90 0))
(define TIMER-TEXT-SIZE 15)
(define CLOCK-CIRCLE-TYPE "outline")
(define BLACK "black")
(define RED "red")
(define QUARTER-DEGREES-IN-CIRCLE 90)
(define CENTER "center")
(define TOP "top")
(define NEW-SQUARE-EVENT "s")
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define c0 (make-clock HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)))

    (send c0 after-tick)
    (check-equal?
     (send c0 toy-data)
     1
     "Unselected Clock after tick")

    (send c0 after-key-event NEW-SQUARE-EVENT)
    (check-equal?
     (send c0 toy-y)
     HALF-CANVAS-HEIGHT
     "Unselected Clock after key event")

    (send c0 after-button-down (+ HALF-CANVAS-WIDTH CANVAS-WIDTH) HALF-CANVAS-HEIGHT)
    (check-false
     (send c0 for-test:selected?)
     "Clock mouse button down outside")

    (send c0 after-button-up (+ HALF-CANVAS-WIDTH CANVAS-WIDTH) HALF-CANVAS-HEIGHT)
    (check-false
     (send c0 for-test:selected?)
     "Clock mouse button up outside")

    (send c0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-false
     (send c0 for-test:selected?)
     "Selected Clock mouse drag")

    (send c0 after-button-down HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send c0 for-test:selected?)
     "Clock mouse button down inside")

    (send c0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-equal?
     (send c0 toy-x)
     HALF-CANVAS-WIDTH
     "Clock drag event.")
    
    (check-equal?
     (send (make-clock HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT) add-to-scene EMPTY-CANVAS)
     (place-image
      (text (number->string 0)
            TIMER-TEXT-SIZE BLACK)
      HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT
      (scene+line
       (place-image 
        (foldr
         (lambda (elt d)
           (rotate
            QUARTER-DEGREES-IN-CIRCLE
            (overlay/align CENTER TOP (text (number->string elt) DIAL-TEXT BLACK) d)))
         (circle CLOCK-RADIUS CLOCK-CIRCLE-TYPE BLACK)
         DIGITS-LIST) HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT EMPTY-CANVAS)
       HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT HALF-CANVAS-WIDTH
       (- HALF-CANVAS-HEIGHT HAND-RADIUS)
       RED))
     "Clock to scene")))