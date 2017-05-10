#lang racket
;; PlaygroundState.rkt : Class definition for PlaygroundState class.

;; Require statements:
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")
(require "Target.rkt")
(require "Square.rkt")
(require "Throbber.rkt")
(require "Football.rkt")
(require "Clock.rkt")

;; Provide
(provide PlaygroundState%
         make-playground)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

;; A ListofToy is one of
;; -- empty
;; -- (cons Toy ListofToy)
;; TEMPLATE:
;; lot-fn : ListofToy -> ??
#|
(define (lot-fn lot)
  (cond
    [(empty? lot) ...]
    [else (... (first lot)
               (lot-fn (rest lot)))]))
|#

;; A MaybeToy is one of
;; -- false
;; -- Toy
;; TEMPLATE:
;; mbt-fn : MaybeToy -> ??
#|
(define (mbt-fn mbt)
  (cond
    [(false? mx) ...]
    [else (... mbt)]))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; A PlaygroundState is a
;; (new PlaygroundState% [target Target][toys ListofToy][velocity Integer])
;; A PlaygroundState represents a playground in which target and toys lives.

(define PlaygroundState%
  (class* object% (PlaygroundState<%>)
    
    ;; Target object which is represented as circle on canvas
    (init-field target)
    
    ;; ListofToy present in the Playground
    (init-field toys)
    
    ;; square toys created in the future will travel with this velocity
    (init-field velocity)
    
    ;; Key Event for new square toy
    (field [NEW-SQUARE-EVENT "s"])
    
    ;; Key Event for new throbber toy
    (field [NEW-THROBBER-EVENT "t"])
    
    ;; Key Event for new clock toy
    (field [NEW-CLOCK-EVENT "w"])
    
    ;; Key Event for new football toy
    (field [NEW-FOOTBALL-EVENT "f"])
    
    (super-new)
    
    
    ;; target-x:   -> Integer
    ;; target-y:   -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the x and y coordinates of the target
    ;; EXAMPLE: a Target with center at (10,14) will return 10 for target-x function
    ;;          and will return 14 for target-y function.
    (define/public (target-x)
      (send target target-x))
    
    (define/public (target-y)
      (send target target-y))
    
    
    ;; target-selected?:  -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true iff the target is selected?, otherwise false
    ;; EXAMPLE: a selected Target will return true.
    (define/public (target-selected?)
      (send target target-selected?))
    
    
    ;; get-toys: -> ListOfToy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a list of Toy<%> present in PlaygroundState
    (define/public (get-toys) toys)
    
    
    ;; after-tick : -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this playground to the state it should have following a tick.
    ;; EXAMPLE: consider a PlaygroundState p1, the function after-tick update p1 to the
    ;;          state where all the toys and target in the playground have a state after
    ;;          one-tick.(See tests below)
    ;; STRATEGY: Use HOF for-each on the toys in this Playground   
    (define/public (after-tick)
      (begin
        (send target after-tick)
        (for-each
         ;; Toy<%> -> Void
         ;; GIVEN: a toy
         ;; EFFECT: updates the given toy to the state it should have following a tick. 
         (lambda (obj)
           (send obj after-tick))
         toys)))
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with target and toys of this playground
    ;;          painted on it.
    ;; EXAMPLE: consider a PlaygroundState p1, the function add-to-scene will return a
    ;;          scene with all toys and target in playground p1 painted on it.
    ;;          (See tests below)
    ;; STRATEGY: Use HOF foldr on the toys in this Playground
    (define/public (add-to-scene scene)
      (foldr
       ;; Toy<%> Scene -> Scene
       ;; GIVEN: a toy and a scene
       ;; RETURNS: a scene like given one with the given toy painted on it. 
       (lambda (obj scene)
         (send obj add-to-scene scene))
       (send target add-to-scene scene)
       toys))
    
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this playground to the state it should have following the given
    ;;         key event
    ;; EXAMPLE: consider a PlaygroundState p1, the function after-key-event will update p1
    ;;      to the state where a new toy based on the key event added to toys of p1.
    ;;      if key event is NEW-SQUARE-EVENT then new toy will be a Square
    ;;      if key event is NEW-THROBBER-EVENT then new toy will be a Throbber
    ;;      if key event is NEW-CLOCK-EVENT then new toy will be a Clock
    ;;      if key event is NEW-FOOTBALL-EVENT then new toy will be a Football
    ;;      (See tests below)
    ;; STRATEGY: Combine simpler functions
    (define/public (after-key-event kev)
      (set! toys (toys-after-key-event kev)))
    
    
    ;; toys-after-key-event: KeyEvent-> ListofToy<%>
    ;; GIVEN: a Key Event
    ;; RETURNS: the list of Toys<%> with a new Toy<%> appended if
    ;;          given key event is one of NEW-SQUARE-EVENT,NEW-THROBBER-EVENT,
    ;;          NEW-CLOCK-EVENT,NEW-FOOTBALL-EVENT
    ;; EXAMPLE: (See tests below)
    ;; STRATEGY: Divide into cases on value of toy
    (define (toys-after-key-event kev)
      (local
        ((define toy (new-toy kev)))
        (if (false? toy)
            toys
            (cons toy toys))))
    
    
    ;; new-toy: KeyEvent -> MayBeToy
    ;; GIVEN: a Key Event
    ;; RETURNS: a new Toy<%> as per a given key event, if the key event is not one of
    ;;          NEW-SQUARE-EVENT,NEW-THROBBER-EVENT,NEW-CLOCK-EVENT,NEW-FOOTBALL-EVENT
    ;;          then returns false.
    ;; EXAMPLE: (See tests below)
    ;; STRATEGY: Divide into cases on KeyEvent kev 
    (define (new-toy kev)
      (cond
        [(key=? kev NEW-SQUARE-EVENT) (make-square-toy (target-x) (target-y) velocity)]
        [(key=? kev NEW-THROBBER-EVENT) (make-throbber (target-x) (target-y))]
        [(key=? kev NEW-CLOCK-EVENT) (make-clock (target-x) (target-y))]
        [(key=? kev NEW-FOOTBALL-EVENT) (make-football (target-x) (target-y))]
        [else false]))
    
    
    ;; after-button-down: Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this widget to the state it should have following the
    ;;         button down mouse event at the given location.
    ;; EXAMPLE: consider a playground state p1, after a mouse button down event at center
    ;;          of canvas, it should update the p1 to the state where all toys and target
    ;;          selected if the mouse position is inside them. 
    ;; STRATEGY: Ue HOF for-each on toys of this playground
    (define/public (after-button-down mx my)
      (begin
        (send target after-button-down mx my)
        (for-each
         ;; Toy<%> -> Void
         ;; GIVEN: a toy
         ;; EFFECT: updates the given toy to the state it should have following a button
         ;; down event. 
         (lambda (obj)
           (send obj after-button-down mx my))
         toys)))
    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this widget to the state it should have following the button up
    ;;         mouse at the given location.
    ;; EXAMPLE: consider a playground state p1, after a mouse button up event at center of
    ;;          canvas, it should update the p1 to the state where all toys and target got
    ;;          unselected. 
    ;; STRATEGY: Ue HOF for-each on toys of this playground
    (define/public (after-button-up mx my)
      (begin
        (send target after-button-up mx my)
        (for-each
         ;; Toy<%> -> Void
         ;; GIVEN: a toy
         ;; EFFECT: updates the given toy to the state it should have following a button
         ;; up event. 
         (lambda (obj)
           (send obj after-button-up mx my))
         toys)))
    
    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this widget to the state it should have following the drag mouse
    ;;         event at the given location.
    ;; EXAMPLE: consider a playground state p1, after a mouse drag event at center of
    ;;          canvas, it should update the p1 to the state where all the selected toys
    ;;          and target dragged. 
    ;; STRATEGY: Ue HOF map on toys
    (define/public (after-drag mx my)
      (begin
        (send target after-drag mx my)
        (for-each
         ;; Toy<%> -> Void
         ;; GIVEN: a toy
         ;; EFFECT: updates the given toy to the state it should have following a drag
         ;; event. 
         (lambda (obj)
           (send obj after-drag mx my))
         toys)))
    ))

;; make-playground : PosInt -> PlaygroundState<%>
;; GIVEN: intial speed of all square toys present in playground 
;; RETURNS: a playground with a target, but no toys, and in which any square toys created
;;          in the future will travel at the given speed (in pixels/tick). 
(define (make-playground v)
  (new PlaygroundState% [target (make-target)][toys empty][velocity v]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS FOR TESTING:

;; KeyEvents:
(define NEW-SQUARE-EVENT "s")
(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "w")
(define NEW-FOOTBALL-EVENT "f")

;; MouseEvents:
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")

(define SQUARE-SIDE 40)
(define SQUARE-SPEED 5)
(define SQUARE-MODE "outline")
(define SQUARE-COLOR "blue")
(define SEL-SQUARE-COLOR "red")
(define TARGET-RADIUS 10)
(define TARGET-MODE "outline")
(define TARGET-COLOR "blue")
(define SEL-TARGET-COLOR "red")

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define p0 (make-playground SQUARE-SPEED))
     (define p1 (make-playground SQUARE-SPEED)))
    
    (check-equal?
     (send p0 get-toys)
     empty
     "Playground without toys")    
    
    (send p0 after-key-event NEW-FOOTBALL-EVENT)
    (check-equal?
     (send (first (send p0 get-toys)) toy-data)
     10
     "Playground after new football event.")
    
    (send p0 after-key-event NEW-CLOCK-EVENT)
    (check-equal?
     (send (first (send p0 get-toys)) toy-data)
     0
     "Playground after new clock event.")
    
    (send p0 after-key-event NEW-THROBBER-EVENT)
    (check-equal?
     (send (first (send p0 get-toys)) toy-data)
     5
     "Playground after new throbber event.")
    
    (send p0 after-key-event NEW-SQUARE-EVENT)
    (check-equal?
     (send (first (send p0 get-toys)) toy-data)
     SQUARE-SPEED
     "Playground after new square event.")
    
    (send p0 after-key-event "q")
    (send p0 after-tick)
    (check-equal?
     (send p0 target-x)
     HALF-CANVAS-WIDTH
     "Playground after tick.")
    
    (check-equal?
     (send p0 target-y)
     HALF-CANVAS-HEIGHT
     "Playground after tick.")    
    
    (send p0 after-button-down (add1 (+ HALF-CANVAS-WIDTH TARGET-RADIUS))
          HALF-CANVAS-HEIGHT)    
    (check-false
     (send p0 target-selected?)
     "Playground mouse button down outside")
    
    (send p0 after-button-down HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-true
     (send p0 target-selected?)
     "Playground mouse button down inside")
    
    (send p0 after-button-up HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-false
     (send p0 target-selected?)
     "Playground mouse button up")
    
    (send p0 after-drag HALF-CANVAS-WIDTH HALF-CANVAS-HEIGHT)
    (check-false
     (send p0 target-selected?)
     "Playground mouse drag inside")
    
    (send p0 after-drag (add1 (+ HALF-CANVAS-WIDTH TARGET-RADIUS)) HALF-CANVAS-HEIGHT)
    (check-false
     (send p0 target-selected?)
     "Unselected Playground mouse drag outside")
    
    (send p1 after-key-event NEW-SQUARE-EVENT)
    (check-equal?
     (send p1 add-to-scene EMPTY-CANVAS)
     (place-image
      (square SQUARE-SIDE SQUARE-MODE SQUARE-COLOR)
      HALF-CANVAS-WIDTH
      HALF-CANVAS-HEIGHT
      (place-image
       (circle TARGET-RADIUS TARGET-MODE TARGET-COLOR)
       HALF-CANVAS-WIDTH
       HALF-CANVAS-HEIGHT
       EMPTY-CANVAS))
     "Playground to scene")))