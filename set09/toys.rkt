#lang racket
;; Design a program to make an interactive display of toys of various shapes
;; and different behaviours

;; Require Statements
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

(provide make-world
         run
         make-square-toy
         make-throbber
         make-clock
         make-football
         PlaygroundState<%>
         Toy<%>
         WorldState<%>
         Widget<%>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS:

;; Canvas Properties:
(define CANVAS-HEIGHT 600)
(define CANVAS-WIDTH 500)
(define TARGET-INITIAL-X (/ CANVAS-WIDTH 2))
(define TARGET-INITIAL-Y (/ CANVAS-HEIGHT 2))
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;INTERFACES
;; Every object that lives in the playground must implement the PlaygroundState<%> which
;; inherits the WorldState<%>

(define WorldState<%>
  (interface ()
    
    ;; -> WorldState<%>
    ;; GIVEN: no arguments
    ;; RETURNS: the state of the world at the next tick
    after-tick          
    
    ;; Integer Integer MouseEvent-> WorldState<%>
    ;; GIVEN: a location of mouse as x,y co-ordinates, and the mouse event
    ;; RETURNS: the state of the world that should follow the
    ;; given mouse event at the given location.
    after-mouse-event
    
    
    ;; KeyEvent -> WorldState<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of the world that should follow the
    ;; given key event
    after-key-event     
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene that depicts this World
    to-scene
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Every object that lives in the playground must implement the PlaygroundState<%>
(define PlaygroundState<%>
  (interface (WorldState<%>);; this means: include all the methods in
    ;;  WorldState<%>.    
    
    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURN: the x and y coordinates of center of the target
    target-x
    target-y
    
    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true iff the target is selected?, otherwise false
    target-selected?
    
    ;; -> ListOfToy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a list of Toy<%> present in PlaygroundState%
    get-toys
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Every object that lives in the playground must implement the Widget<%>
;; interface.
(define Widget<%>
  (interface ()
    
    ;; -> Widget<%>
    ;; GIVEN: no arguments
    ;; RETURNS: the state of this object that should follow at time t+1.
    after-tick          
    
    ;; Integer Integer -> Widget<%>
    ;; GIVEN: a location of mouse.
    ;; RETURNS: the state of this object that should follow the
    ;; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ;; KeyEvent -> Widget<%>
    ;; GIVEN: a key event
    ;; RETURNS: the state of this object that should follow the
    ;; given key event
    after-key-event     
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    add-to-scene
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Every target that lives in the playground must implement this interface
(define Target<%>
  (interface (Widget<%>);; this means: include all the methods in
    ;;  Widget<%>. 

    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: the x coordinate of center of target
    target-x

    ;; -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: the y coordinate of center of target
    target-y

    ;; -> Boolean
    ;; GIVEN: no arguments
    ;; RETURNS: true iff the target is selected?, otherwise false
    target-selected?
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; All toys present in the playground must implement this interface
(define Toy<%> 
  (interface (Widget<%>)  ;; this means: include all the methods in
    ;;  Widget<%>. 
    
    ;; -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a square, it is the velocity of the square (rightward is
    ;; positive)
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a football, it is the current size of the football (in
    ;; arbitrary units; bigger is more)
    toy-data
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; make-world: PosInt -> PlaygroundState<%>
;; GIVEN: intial speed of all square toys present in playground 
;; RETURNS: a world with a target, but no toys, and in which any
;;          square toys created in the future will travel at the given speed (in
;;          pixels/tick). 
(define (make-world v)
  (make-playground (new-target) empty v ))

;; new-target: -> Target<%>
;; GIVEN: no arguments
;; RETURNS: a new target object placed at it's intital x,y position.
(define (new-target)
  (new Target% [x TARGET-INITIAL-X][y TARGET-INITIAL-Y]))


;; run : PosNum PosInt  -> PlaygroundState<%> 
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick),
;;        creates and runs a world in which square toys travel at the given
;;        speed.  Returns the final state of the world.
(define (run frame-rate square-speed)
  (big-bang (make-world square-speed)
            (on-tick (lambda (w) (send w after-tick)) frame-rate)
            (on-draw (lambda (w) (send w to-scene)))
            (on-key (lambda (w kev) (send w after-key-event kev)))
            (on-mouse (lambda (w mx my mev) (send w after-mouse-event mx my mev)))))


;; make-square-toy : PosInt PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed
;; RETURNS: an object representing a square toy at the given position,
;; travelling right at the given speed.
(define (make-square-toy x y v)
  (new Square% [x x][y y][velocity v]))


;; make-throbber: PosInt PosInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
(define (make-throbber x y)
  (new Throbber% [x x][y y]))


;; make-clock : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
(define (make-clock x y)
  (new Clock% [x x][y y]))


;; make-football : PosInt PostInt -> Toy<%>
;; GIVEN: an x and a y position
;; RETURNS: an object representing a football at the given position.
(define (make-football x y)
  (new Football% [x x][y y]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLASS DEFINITIONS

;; The PlaygroundState% class
;; A PlaygroundState is a (make-playground Target ListofToys Integer)

;; make-playground: Target ListOfToys Integer -> PlaygroundState<%>
;; GIVEN: a Target object, ListOfToys and a velocity v.
;; RETURNS: a world with a target, with given list of toys, and in which any
;;          square toys created in the future will travel at the given speed (in
;;          pixels/tick).
(define (make-playground target toys v)
  (new PlaygroundState% [target target][toys toys][velocity v]))

(define PlaygroundState%
  (class* object% (PlaygroundState<%>)
    
    (init-field target) ; Target object which is represented as circle on canvas
    (init-field toys) ;  ListOfToys present in the Playground
    (init-field velocity) ; square toys created in the future will travel with this
                          ; velocity

    ;; Key Event for new square toy
    (field [NEW-SQUARE-EVENT "s"])

    ;; Key Event for new throbber toy
    (field [NEW-THROBBER-EVENT "t"])

    ;; Key Event for new clock toy
    (field [NEW-CLOCK-EVENT "w"])

    ;; Key Event for new football toy
    (field [NEW-FOOTBALL-EVENT "f"])

    ;; Mouse Event for button-down
    (field [BUTTON-DOWN-EVENT "button-down"])
    
    ;; Mouse Event for drag
    (field [DRAG-EVENT "drag"])
    
    ;; Mouse Event for button-up
    (field [BUTTON-UP-EVENT "button-up"])
    
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

    ;; after-tick: -> PlaygroundState<%>
    ;; GIVEN: no arguments
    ;; RETURNS: the state of the world at the next tick
    ;; EXAMPLE: consider a PlaygroundState p1, the function after-tick will return
    ;;          the new state p-tick where all the toys and target in the playground have
    ;;          a state after one-tick.(See tests below)
    ;; STRATEGY: Use HOFC map on the toys in this Playground
    (define/public (after-tick)
      (make-playground
       (send target after-tick)
       (map
        (lambda (toy) (send toy after-tick))
        toys)
       velocity))

    ;; to-scene : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene that depicts this World
    ;; EXAMPLE: consider a PlaygroundState p-key-1, the function to-scene will return
    ;;          a new scene with all toys and target in playground painted on it
    ;;          (See tests below)
    ;; STRATEGY: Use HOFC foldr on the toys in this Playground
    (define/public (to-scene)
      (foldr
       (lambda (obj scene)
         (send obj add-to-scene scene))
       (send target add-to-scene EMPTY-CANVAS)
       toys))
    
    ;; after-key-event : KeyEvent -> PlaygroundState<%>
    ;; GIVEN: a key event kev
    ;; RETURNS: the state of the world after a given key event
    ;; EXAMPLE: consider a PlaygroundState p0, the function after-key-event will return
    ;;          a new PlaygroundState p-key-1 with a new Square toy added to p0
    ;;          if key event NEW-SQUARE-EVENT is passed else if
    ;;          a new PlaygroundState p-key-2 with a new Throbber toy added to p0
    ;;          if key event NEW-THROBBER-EVENT is passed else if
    ;;          a new PlaygroundState p-key-3 with a new Clock added to p0
    ;;          if key event NEW-CLOCK-EVENT is passed else if
    ;;          a new PlaygroundState p-key-4 with a new Football toy added to p0
    ;;          if key event NEW-FOOTBALL-EVENT is passed else if
    ;;          any other key event is passed same playground p0 will be returned.
    ;;          (See tests below)
    ;; STRATEGY: Combine simpler functions
    (define/public (after-key-event kev)
      (make-playground target (toys-after-key-event kev) velocity))

    ;; toys-after-key-event: KeyEvent-> ListOfToys<%>
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
    ;; STRATEGY: Divide into cases on KeyEvent mev 
    (define (new-toy kev)
      (cond
        [(key=? kev NEW-SQUARE-EVENT) (make-square-toy (target-x) (target-y) velocity)]
        [(key=? kev NEW-THROBBER-EVENT) (make-throbber (target-x) (target-y))]
        [(key=? kev NEW-CLOCK-EVENT) (make-clock (target-x) (target-y))]
        [(key=? kev NEW-FOOTBALL-EVENT) (make-football (target-x) (target-y))]
        [else false]))
    
    ;; world-after-mouse-event : NonNegInt NonNegInt MouseEvent -> PlaygroundState<%>
    ;; GIVEN :a Mouse event mev, and x,y co-ordinates of mouse
    ;; RETURNS: the PlaygroundState after a given mouse event
    ;; EXAMPLE: consider a playground state p1 after a mouse button down event at
    ;;          center of canvas, it should return same playground as given but with all
    ;;          toys, target selected if the mouse position is inside them.In this example
    ;;          it should return p-but-down-1
    ;; STRATEGY: Divide into cases on MouseEvent mev
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev BUTTON-DOWN-EVENT) (world-after-button-down mx my)]
        [(mouse=? mev DRAG-EVENT) (world-after-drag mx my)]
        [(mouse=? mev BUTTON-UP-EVENT) (world-after-button-up mx my)]
        [else this]))
    
    ;; the next few functions are local functions, not in the interface.

    ;; world-after-button-down: NonNegInt NonNegInt -> PlaygroundState<%>
    ;; GIVEN:  x,y co-ordinates of mouse
    ;; RETURNS: the PlaygroundState after button down mouse event
    ;; EXAMPLE: consider a playground state p1 after a mouse button down event at
    ;;          center of canvas, it should return same playground as given but with all
    ;;          toys, target selected if the mouse position is inside them.In this example
    ;;          it should return p-but-down-1 
    ;; STRATEGY: Ue HOF map on toys 
    (define (world-after-button-down mx my)
      (make-playground
       (send target after-button-down mx my)
       (map
        (lambda (toy) (send toy after-button-down mx my))
        toys)
       velocity))
    
    ;; world-after-button-up: NonNegInt NonNegInt -> PlaygroundState<%>
    ;; GIVEN:  x,y co-ordinates of mouse
    ;; RETURNS: the PlaygroundState after button up mouse event
    ;; EXAMPLE: consider a playground state p1 after a mouse button up event at
    ;;          center of canvas, it should return same playground as given but with all
    ;;          toys, target unselected if the mouse position is inside them.
    ;;          In this example it should return p-but-up-1 
    ;; STRATEGY: Ue HOF map on toys 
    (define (world-after-button-up mx my)
      (make-playground
       (send target after-button-up mx my)
       (map
        (lambda (toy) (send toy after-button-up mx my))
        toys)
       velocity))

    ;; world-after-drag: NonNegInt NonNegInt -> PlaygroundState<%>
    ;; GIVEN:  x,y co-ordinates of mouse
    ;; RETURNS: the PlaygroundState after drag mouse event
    ;; EXAMPLE: consider a playground state p1 after a mouse drag event at
    ;;          center of canvas, it should return same playground as given but with all
    ;;          toys, target which are selected dragged by a distance which the mouse
    ;;          drags. In this example it should return p-drag-1 
    ;; STRATEGY: Ue HOF map on toys
    (define (world-after-drag mx my)
      (make-playground
       (send target after-drag mx my)
       (map
        (lambda (toy) (send toy after-drag mx my))
        toys)
       velocity))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Target% class
;; A Target is a (make-target Integer Integer Boolean Integer Integer)

;; make-target: Integer Integer Boolean Integer Integer -> Target%
;; GIVEN: co-ordinates of center of target, a boolean representing whether the target is
;;        selected, if the Target is selected, the position of the last button-down event
;;        inside the Target, relative to the Target's center.  Else any value.
;; RETURNS: a new Target<%> with the given features.
(define (make-target x y selected saved-mx saved-my)
  (new Target% [x x][y y][selected? selected][saved-mx saved-mx][saved-my saved-my]))

(define Target%
  (class* object% (Target<%>)
    
    ;; the init-fields are the values that may vary from one Target to
    ;; the next.
    
    ; the x and y position of the center of the Target
    (init-field x y)   
    
    ; is the Target selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the Target is selected, the position of
    ;; the last button-down event inside the Target, relative to the
    ;; Target's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
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
    
    ;; after-tick : -> Target<%>
    ;; Given: no arguments
    ;; RETURNS: A Target like this one.
    ;; DETAILS: a Target does not change after each tick
    (define/public (after-tick) this)
    
    ;; after-key-event : KeyEvent -> Target<%>
    ;; GIVEN: a key event
    ;; RETURNS: A Target like this one.
    ;; DETAILS: a Target ignores key events
    (define/public (after-key-event kev) this)      
    
    ;; after-button-down : Integer Integer -> Target<%>
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A Target like this but after button down mouse event
    ;; EXAMPLE: Consider a target t0 is present in playground. Once a mouse button
    ;;         down event occurs , and the mouse pointer is inside the circle then
    ;;         target gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the Target
    (define/public (after-button-down mx my)
      (if (in-target? mx my)
          (make-target x y true (- mx x) (- my y))
          this))
    
    ;; after-button-up : Integer Integer -> Target<%>
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A Target like this but after button up mouse event 
    ;; EXAMPLE: Consider a target t0 which is present in playground. Once a mouse
    ;;          button up event occurs, and the mouse pointer is inside the circle
    ;;          then target gets unselected if its selected.
    ;; STRATEGY: Cases on whether the mouse is in the Target.
    (define/public (after-button-up mx my)
      (if (in-target? mx my)
          (make-target x y false saved-mx saved-my)
          this))   
    
    ;; after-drag : Integer Integer -> Target<%>
    ;; GIVEN: the location of a drag event
    ;; RETURNS:  Target like this but after drag mouse event
    ;; EXAMPLE: If target is selected, moves it so that the vector from the center to
    ;; the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether the Target is selected.
    (define/public (after-drag mx my)
      (if selected?
          (make-target (- mx saved-mx) (- my saved-my) true saved-mx saved-my)
          this))   
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a Scene like the given one, but with this Target painted
    ;; on it.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Square% class
;; A Square is a (make-square Integer Integer Integer Boolean Integer Integer)

;; make-square: Integer Integer Integer Boolean NonNegInt NonNegInt -> Toy<%>
;; GIVEN: an x and a y position, and a speed v , a boolean representing whether the
;;        square is selected, if the Square is selected, the position of the
;;        last button-down event inside the Square, relative to the Square's center.
;;        Else any value.
;; RETURNS: an object representing a square toy at the given position,
;;          travelling right at the given speed.
(define (make-square x y v selected mx my)
  (new Square% [x x][y y][velocity v] [selected? selected] [saved-mx mx] [saved-my my]))

(define Square%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one square to
    ;; the next.
    
    ;; the x and y position of the center of the square
    (init-field x y)

    ;; the velocity of square along x-axis 
    (init-field velocity)
    
    ;; is the square selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the square is selected, the position of
    ;; the last button-down event inside the square, relative to the
    ;; sqaure's center.  Else any value.
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
    
    ;; after-tick : -> Toy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: A Square like this one, but as it should be after a tick
    ;;          a selected sqaure doesn't move.
    ;; EXAMPLE: the function returns a square after moving horizontally as per its
    ;;          velocity. When its edge reaches the edge of the canvas, it executes
    ;;          a Perfect Bounce.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (make-square (square-x-after-tick)
                       (square-y-after-tick)
                       (velocity-after-tick)
                       selected?
                       saved-mx
                       saved-my)))
    
    ;; square-x-after-tick: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Value of x-coordinate of center of this square after a tick
    ;; EXAMPLE: if square's edge touches or go past the right side of canvas it
    ;;          returns max-x, if square's edge touches or go past the left side
    ;;          of canvas it returns min-x else it returns the sum of its current
    ;;          x-coordinate and velocity
    ;; STRATEGY: Divide into cases on whether square has passed left or right border of
    ;;           canvas
    (define (square-x-after-tick)
      (cond
        [(square-touch-or-go-past-left-side?) MIN-X]
        [(square-touch-or-go-past-right-side?) MAX-X]
        [else (+ x velocity)]))

    ;; square-y-after-tick: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Value of y-coordinate of center of this square after a tick
    ;; EXAMPLE: if square's edge touches or go past the bottom side of canvas it
    ;;          returns max-y, if square's edge touches or go past the top side
    ;;          of canvas it returns min-y else it returns the its current y-coordinate
    ;; STRATEGY: Divide into cases on whether square has passed top or bottom border of
    ;;           canvas
    (define (square-y-after-tick)
      (cond
        [(square-touch-or-go-past-top-side?) MIN-Y]
        [(square-touch-or-go-past-bottom-side?) MAX-Y]
        [else y]))

    ;; velocity-after-tick: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: Value of velocity this square after a tick
    ;; EXAMPLE: if square's edge touches or go past the right or left side of canvas it
    ;;          returns the negative of current velocity else returns the same velocity
    ;; STRATEGY: Divide into cases on whether square has passed left or right border of
    ;;           canvas
    (define (velocity-after-tick)
      (if (or (square-touch-or-go-past-left-side?)
              (square-touch-or-go-past-right-side?))
          (- velocity)
          velocity))
    
    ;; square-touch-or-go-past-left-side?:   -> Boolean
    ;; square-touch-or-go-past-right-side?:  -> Boolean
    ;; square-touch-or-go-past-top-side?:    -> Boolean
    ;; square-touch-or-go-past-bottom-side?: -> Boolean
    ;; GIVEN: no arguments
    ;; ANSWERS: whether the square is touching or going past the
    ;;          specific side after a tick.
    ;; DESIGN STRATEGY: Combine simpler functions
    (define (square-touch-or-go-past-left-side?)
      (<= (+ x velocity) MIN-X))
    
    (define (square-touch-or-go-past-right-side?)
      (>= (+ x velocity) MAX-X))

    (define (square-touch-or-go-past-top-side?)
      (<= y MIN-Y))
    
    (define (square-touch-or-go-past-bottom-side?)
      (>= y MAX-Y))
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a KeyEvent kev
    ;; RETURNS: A square like this but in a state as it should be after a key event
    ;; DETAILS: a square ignores key events
    (define/public (after-key-event kev)
      this)      

    ;; after-button-down : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A square like this but in a state as it should be after a
    ;;         mouse button down event
    ;; EXAMPLE: Consider a square s0 which is present in playground. Once a mouse
    ;;          button down event occurs, and the mouse pointer is inside the square,
    ;;          square gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the square
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
          (make-square x y velocity true (- mx x) (- my y))
          this))
    
    ;; after-button-up : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A square like this but in a state as it should be after a
    ;;         mouse button up event
    ;; EXAMPLE: Consider a square s0 which is present in playground. Once a mouse
    ;;          button up event occurs, and the mouse pointer is inside the square,
    ;;          square gets unselected.
    ;; STRATEGY: Cases on whether the mouse is in the square
    (define/public (after-button-up mx my)
      (if (in-square? mx my)
          (make-square x y velocity false saved-mx saved-my)
          this))   
    
    ;; after-drag : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A square like this but in a state as it should be after a
    ;;         mouse drag event
    ;; EXAMPLE: If square is selected, moves it so that the vector from the center to
    ;; the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether square is selected
    (define/public (after-drag mx my)
      (if selected?
          (make-square (- mx saved-mx)
                       (- my saved-my)
                       velocity
                       true
                       saved-mx
                       saved-my)
          this))   
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this square painted
    ;; on it.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Throbber% class
;; A Throbber is a
;;(make-throbb Integer Integer Boolean Integer Integer PosInt (Number Number -> Number))

;; make-throbb:
;; Integer Integer Boolean Integer Integer PosInt (Number Number -> Number) -> Toy<%>
;; GIVEN: an x and a y position , a boolean representing whether the
;;        throbber is selected, if the Throbber is selected, the position of the last
;;        button-down event inside the Throbber, relative to the Throbber's center.
;;        Else any value. the radius of throbber r, and an operator to be applied on
;;        radius after each tick
;; RETURNS: an object representing a throbber toy at the given position,
;;          with radius r.
(define (make-throbb x y selected mx my r func)
  (new Throbber%
       [x x]
       [y y]
       [selected? selected]
       [saved-mx mx]
       [saved-my my]
       [r r]
       [operator func]))

(define Throbber%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one Throbber to
    ;; the next.
    
    ; the x and y position of the center of the Throbber
    (init-field x y)
    
    ; is the Throbber selected? Default is false.
    (init-field [selected? false]) 
    
    ;; if the Throbber is selected, the position of
    ;; the last button-down event inside the Throbber, relative to the
    ;; Throbber's center.  Else any value.
    (init-field [saved-mx 0] [saved-my 0])

    ;; the Throbber's radius set to default as THROBBER-RADIUS
    (init-field [r 5])

    ;; the operator to be applied on radius at next tick (+ / -)
    ;; Default is positive
    (init-field [operator +])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ;; the minimum radius to which a throbber shrinks
    (field [MIN-R 5])

    ;; the maximum radius to which a throbber expands
    (field [MAX-R 20])

    ;; the rate at which throbber expands/contracts
    (field [THROBBER-EXP-CON-RATE 1])
    
    ;; image for displaying the unselected throbber
    (field [THROBBER-IMG (circle r "solid" "green" )])

    ;; image for displaying the selected throbber
    (field [SEL-THROBBER-IMG (circle r "solid" "red")])
    
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
    
    ;; after-tick :  -> Toy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: A Throbber like this one, but as it should be after a tick
    ;; a selected throbber doesn't move.
    ;; EXAMPLE: the function returns a throbber which has expanded/shrinked by throbber's
    ;;          expansion/contraction rate. if the operator is + then it expands else it
    ;;          contracts. Once it reaches to its minimum radius, operator reversed to +
    ;;          from - and once it reaches to maximum radius it reversed to - from +.
    ;;          Selected throbber will remain in the same state after tick.
    ;; STRATEGY: Cases on selected?
    (define/public (after-tick)
      (if selected?
          this
          (make-throbb x
                       y
                       selected?
                       saved-mx
                       saved-my
                       (operator r THROBBER-EXP-CON-RATE)
                       (operator-after-tick))))

    ;; operator-after-tick: -> (Number Number -> Number) 
    ;; GIVEN: no arguments
    ;; RETURNS: the operator to be applied on radius after tick
    ;; EXAMPLE: the function returns a function + or -. If the radius reaches to its
    ;;          minimum value then operator reversed to + from -, and once it reaches
    ;;          to maximum radius it reversed to - from +.
    ;; STRATEGY: Divide into cases on value of radius after applying operator
    (define (operator-after-tick)
      (cond
        [(= (operator r THROBBER-EXP-CON-RATE) MIN-R) +]
        [(= (operator r THROBBER-EXP-CON-RATE) MAX-R) -]
        [else operator]))
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a KeyEvent kev
    ;; RETURNS: A throbber like this but in a state as it should be after a key event
    ;; DETAILS: a throbber ignores key events
    (define/public (after-key-event kev) this)      
 
    ;; after-button-down : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A throbber like this but in a state as it should be after a
    ;;         mouse button down event
    ;; EXAMPLE: Consider a throbber t0 which is present in playground. Once a mouse
    ;;          button down event occurs, and the mouse pointer is inside the throbber,
    ;;          throbber gets selected.
    ;; STRATEGY: Cases on whether the mouse is in the throbber
    (define/public (after-button-down mx my)
      (if (in-throbber? mx my)
          (make-throbb x y true (- mx x) (- my y) r operator)
          this))
    
    ;; after-button-up : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A throbber like this but in a state as it should be after a
    ;;         mouse button up event
    ;; EXAMPLE: Consider a throbber t0 which is present in playground. Once a mouse
    ;;          button up event occurs, and the mouse pointer is inside the throbber,
    ;;          it gets unselected.
    ;; STRATEGY: Cases on whether the mouse is in the throbber
    (define/public (after-button-up mx my)
      (if (in-throbber? mx my)
          (make-throbb x y false saved-mx saved-my r operator)
          this))  
    
    ;; after-drag : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A throbber like this but in a state as it should be after a
    ;;         mouse drag event
    ;; EXAMPLE: If throbber is selected, moves it so that the vector from the center to
    ;; the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether throbber is selected
    (define/public (after-drag mx my)
      (if selected?
          (make-throbb (- mx saved-mx) (- my saved-my) true saved-mx saved-my r operator)
          this))   
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;; on it.
    ;; EXAMPLE: Suppose we have an empty canvas then function will paint a solid green
    ;;          circle of initial radius on canvas.          
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image (if selected? SEL-THROBBER-IMG THROBBER-IMG)
                   x
                   y
                   scene))

    ;; test methods, to probe the throbber state.  
    ;; -> Boolean
    (define/public (for-test:selected?)
      selected?)

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Football% class
;; A Football is a
;;(make-ball Integer Integer Boolean Integer Integer Number)

;; make-ball:
;; Integer Integer Boolean Integer Integer Number -> Toy<%>
;; GIVEN: an x and a y position, a boolean representing whether the
;;        football is selected, if the Football is selected, the position of the last
;;        button-down event inside the Football, relative to the Football's center,
;;        else any value and the size of football s.
;; RETURNS: an object representing a football toy at the given position,
;;          with size scaled by s.
(define (make-ball x y selected mx my s)
  (new Football% [x x] [y y] [saved-mx mx] [saved-my my][size s][selected? selected]))

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
    
    ;; if the Football is selected, the position of
    ;; the last button-down event inside the Football, relative to the
    ;; Football's center.  Else any value.
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
    
    ;; after-tick :  -> Toy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: A Football like this one, but as it should be after a tick
    ;; a selected football doesn't move.
    ;; EXAMPLE: the function returns a football which has shrinked by another 10%
    ;;          than original. If the football reaches a sizeless than 10%, from next tick
    ;;          it is not shrinked.So if we have a football enclosed in square of side 100
    ;;          at next tick we will have football of side 90.
    ;; STRATEGY: Cases on selected? and value of size
    (define/public (after-tick)
      (if (or selected? (< size MIN-SCALE-SIZE))
          this
          (make-ball x y selected? saved-mx saved-my (- size MIN-SCALE-SIZE))))
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a KeyEvent kev
    ;; RETURNS: A football like this but in a state as it should be after a key event
    ;; DETAILS: a football ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ;; after-button-down : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A football like this but in a state as it should be after a
    ;;         mouse button down event
    ;; EXAMPLE: Consider a football f0 which is present in playground. Once a mouse
    ;;          button down event occurs , and the mouse pointer is inside canvas the
    ;;         football gets selected if its size is greater than lowest size.
    ;; STRATEGY: Cases on whether the mouse is in the football and value of size
    (define/public (after-button-down mx my)
      (if (and (in-football? mx my)
               (>= size MIN-SCALE-SIZE))
          (make-ball x y true (- mx x) (- my y) size)
          this))
    
    ;; after-button-up : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A football like this but in a state as it should be after a
    ;;         mouse button up event
    ;; EXAMPLE: Consider a football f0 which is present in playground. Once a mouse
    ;;          button up event occurs , and the mouse pointer is inside canvas the
    ;;         football gets unselected if its size is greater than lowest size.
    ;; STRATEGY: Cases on whether the mouse is in the football and value of size
    (define/public (after-button-up mx my)
      (if (and (in-football? mx my)
               (>= size MIN-SCALE-SIZE))
          (make-ball x y false saved-mx saved-my size)
          this))   
    
    ;; after-drag : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A football like this but in a state as it should be after a
    ;;         mouse drag event
    ;; EXAMPLE: If football is selected, moves it so that the vector from the center to
    ;; the drag event is equal to (mx, my).
    ;; STRATEGY: Cases on whether football is selected
    (define/public (after-drag mx my)
      (if (and selected? (>= size MIN-SCALE-SIZE))
          (make-ball (- mx saved-mx) (- my saved-my) true saved-mx saved-my size)
          this))   
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this football painted if it is above
    ;;          size limit
    ;; on it.
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
    (define/public (for-test:selected?)
      selected?)

    ;; in-football? : Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this football.
    ;; STRATEGY: Combine simpler functions
    (define (in-football? other-x other-y)
      (and (<= (- x HALF-IMG-WIDTH) other-x (+ x HALF-IMG-WIDTH))
           (<= (- y HALF-IMG-HEIGHT) other-y (+ y HALF-IMG-HEIGHT))))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Clock% class
;; A Clock is a
;;(make-watch Integer Integer Boolean Integer Integer NonNegInt)

;; make-watch:
;; Integer Integer Boolean Integer Integer NonNegInt -> Toy<%>
;; GIVEN: an x and a y position, a boolean representing whether the
;;        clock is selected,if the clock is selected, the position of the last
;;        button-down event inside the Clock, relative to the Clock's center,
;;        ese any value, and the time since clock was created t.
;; RETURNS: an object representing a clock toy at the given position,
;;          with size scaled by s.

(define (make-watch x y selected mx my t)
  (new Clock%
           [x x]
           [y y]
           [saved-mx mx]
           [saved-my my]
           [selected? selected]
           [timer t]))

(define Clock%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one Clock to
    ;; the next.
    
    ;; the x and y position of the center of the Clock
    (init-field x y)   

    ;; the timer represents time in ticks since clock was created
    (init-field [timer 0]) 

    ;; is the Clock selected? Default is false. 
    (init-field [selected? false])
    
    ;; if the clock is selected, the position of
    ;; the last button-down event inside the Clock, relative to the
    ;; Clock's center.  Else any value.
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
    
    ;; after-tick :  -> Toy<%>
    ;; GIVEN: no arguments
    ;; RETURNS: A Clock like this one, but as it should be after a tick
    ;; STRATEGY: Combine simpler functions
    ;; EXAMPLE: Consider a clock c0 which has beencreated since 109 ticks. At next tick
    ;;          the function should return same clock like c0 but with
    ;;          timer updated to 110 
    (define/public (after-tick)
      (make-watch x y selected? saved-mx saved-my (add1 timer)))
    
    ;; after-key-event : KeyEvent -> Toy<%>
    ;; GIVEN: a KeyEvent kev
    ;; RETURNS: A clock like this but in a state as it should be after a key event
    ;; DETAILS: a clock ignores key events
    (define/public (after-key-event kev)
      this)      
    
    ;; after-button-down : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A clock like this but in a state as it should be after a
    ;;         mouse button down event.
    ;; EXAMPLE: consider a unselected clock c0 placed at center of canvas. Suppose a mouse
    ;;          down event occurs inside the clock circle, the function will return a
    ;;          clock like c0 but now in selected state
    ;; STRATEGY: Cases on whether the mouse is in the clock
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (make-watch x y true (- mx x) (- my y) timer)
          this))
    
    ;; after-button-up : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A clock like this but in a state as it should be after a
    ;;         mouse button up event
    ;; EXAMPLE: consider a selected clock c0 placed at center of canvas. Suppose a mouse
    ;;          up event occurs inside the clock circle, the function will return a
    ;;          clock like c0 but now in unselected state.
    ;; STRATEGY: Cases on whether the mouse is in the clock
    (define/public (after-button-up mx my)
      (if (in-clock? mx my)
          (make-watch x y false saved-mx saved-my timer)
          this))   
    
    ;; after-drag : Integer Integer -> Toy<%>
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A clock like this but in a state as it should be after a
    ;;         mouse drag event
    ;; EXAMPLE: If clock c0 is selected,function will move it so that the
    ;;          vector from the center to the drag event is equal to (mx, my)
    ;; STRATEGY: Cases on whether clock is selected
    (define/public (after-drag mx my)
      (if selected?
          (make-watch (- mx saved-mx) (- my saved-my) true saved-mx saved-my timer)
          this))   
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    ;; EXAMPLE: If we are provided an empty canvas function will paint a clock with
    ;;          digits 0 ,90 ,180, 270 as counters and a red clock-hand with the number
    ;;          of ticks since its creation displayed at center of clock.
    ;; STRATEGY: Combine simpler functions
    (define/public (add-to-scene scene)
      (place-image
       (text (number->string timer) TEXT-SIZE TIMER-TEXT-COLOR) x y
       (draw-clock-hand scene)
       ))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Constants for Testing:

;; KeyEvents:
(define NEW-SQUARE-EVENT "s")
(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "w")
(define NEW-FOOTBALL-EVENT "f")

;; MouseEvents:
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")

;; Target Properties:
(define TARGET-RADIUS 10)
(define TARGET-MODE "outline")
(define TARGET-COLOR "blue")
(define SEL-TARGET-COLOR "red")

;; Square Properties:
(define SQUARE-SIDE 40)
(define HALF-SQUARE-SIDE (/ SQUARE-SIDE 2))
(define SQUARE-SPEED 5)
(define SQUARE-MODE "outline")
(define SQUARE-COLOR "blue")
(define SEL-SQUARE-COLOR "red")

;; Throbber Properties:
(define THROBBER-RADIUS 5)
(define THROBBER-MIN-RADIUS 5)
(define THROBBER-MAX-RADIUS 20)
(define THROBBER-MODE "solid")
(define THROBBER-COLOR "green")
(define SEL-THROBBER-COLOR "red")
(define THROBBER-EXP-CON-RATE 1)

;; Football Properties:
(define SIZE-UNIT 10)

;; Clock Properties:
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS for PlaygroundState:

(begin-for-test
  (local
    ((define p0 (make-world SQUARE-SPEED))

     (define p1 (send (send (send (send p0 after-key-event NEW-FOOTBALL-EVENT)
                                  after-key-event NEW-CLOCK-EVENT)
                            after-key-event NEW-THROBBER-EVENT)
                      after-key-event NEW-SQUARE-EVENT))
     
     (define p-tick (send p1 after-tick))

     (define p-key-1 (send p0 after-key-event NEW-SQUARE-EVENT))
     (define p-key-2 (send p0 after-key-event NEW-THROBBER-EVENT))
     (define p-key-3 (send p0 after-key-event NEW-CLOCK-EVENT))
     (define p-key-4 (send p0 after-key-event NEW-FOOTBALL-EVENT))
     (define p-key-5 (send p0 after-key-event "q"))

     (define p-but-down-1 (send p1
                                after-mouse-event
                                TARGET-INITIAL-X TARGET-INITIAL-Y BUTTON-DOWN-EVENT))

     (define p-but-down-2 (send p1
                                after-mouse-event
                                (add1 (+ TARGET-INITIAL-X TARGET-RADIUS))
                                TARGET-INITIAL-Y BUTTON-DOWN-EVENT))
     
     (define p-but-up-1 (send p1
                              after-mouse-event
                              TARGET-INITIAL-X TARGET-INITIAL-Y BUTTON-UP-EVENT))
     
     (define p-but-up-2 (send p1
                              after-mouse-event
                              (add1 (+ TARGET-INITIAL-X TARGET-RADIUS))
                              TARGET-INITIAL-Y BUTTON-UP-EVENT))

     (define p-drag-1 (send p1
                            after-mouse-event
                            TARGET-INITIAL-X TARGET-INITIAL-Y DRAG-EVENT))
     
     (define p-drag-2 (send p1
                            after-mouse-event
                            (add1 (+ TARGET-INITIAL-X TARGET-RADIUS))
                            TARGET-INITIAL-Y
                            DRAG-EVENT))

     (define p-move (send p1
                          after-mouse-event
                          TARGET-INITIAL-X TARGET-INITIAL-Y "move")))
    
    (check-equal?
     (send p-tick target-x)
     TARGET-INITIAL-X
     "Playground after tick.")

    (check-equal?
     (send p-tick target-y)
     TARGET-INITIAL-Y
     "Playground after tick.")
    
    (check-equal?
     (send (first (send p-key-1 get-toys)) toy-data)
     SQUARE-SPEED
     "Playground after new square event.")

    (check-equal?
     (send (first (send p-key-2 get-toys)) toy-data)
     THROBBER-RADIUS
     "Playground after new throbber event.")

    (check-equal?
     (send (first (send p-key-3 get-toys)) toy-data)
     0
     "Playground after new clock event.")

    (check-equal?
     (send (first (send p-key-4 get-toys)) toy-data)
     SIZE-UNIT
     "Playground after new football event.")

    (check-equal?
     (send p-key-5 get-toys)
     empty
     "Playground after other key event.")

    (check-true
     (send p-but-down-1 target-selected?)
     "Playground mouse button down inside")

    (check-false
     (send p-but-down-2 target-selected?)
     "Playground mouse button down outside")

    (check-false
     (send p-but-up-1 target-selected?)
     "Playground mouse button up inside")
    
    (check-false
     (send p-but-up-2 target-selected?)
     "Playground mouse button up outside")
    
    (check-false
     (send p-drag-2 target-selected?)
     "Unselected Playground mouse drag")

    (check-false
     (send p-move target-selected?)
     "Unselected Playground mouse move event")

    (check-equal?
     (send p-key-1 to-scene)
     (place-image
      (square SQUARE-SIDE SQUARE-MODE SQUARE-COLOR)
      TARGET-INITIAL-X
      TARGET-INITIAL-Y
      (place-image
       (circle TARGET-RADIUS TARGET-MODE TARGET-COLOR)
       TARGET-INITIAL-X
       TARGET-INITIAL-Y
       EMPTY-CANVAS))
     "Playground to scene")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS for Target:

(begin-for-test
  (local
    ((define t0 (new-target))
     (define t1 (new Target%
                     [x TARGET-INITIAL-X]
                     [y TARGET-INITIAL-Y]
                     [selected? true]
                     [saved-mx TARGET-INITIAL-X]
                     [saved-my TARGET-INITIAL-Y]))

     (define t-tick-1 (send t0 after-tick))

     (define t-key-1 (send t0 after-key-event NEW-THROBBER-EVENT))

     (define t-but-down-1 (send t0 after-button-down TARGET-INITIAL-X TARGET-INITIAL-Y))

     (define t-but-down-2 (send t0
                                after-button-down
                                (add1 (+ TARGET-INITIAL-X TARGET-RADIUS))
                                TARGET-INITIAL-Y))
     
     (define t-but-up-1 (send t1 after-button-up TARGET-INITIAL-X TARGET-INITIAL-Y))
     
     (define t-but-up-2 (send t0
                              after-button-up
                              (add1 (+ TARGET-INITIAL-X TARGET-RADIUS))
                              TARGET-INITIAL-Y))

     (define t-drag-1 (send t1 after-drag TARGET-INITIAL-X TARGET-INITIAL-Y))
     
     (define t-drag-2 (send t0
                            after-drag
                            (add1 (+ TARGET-INITIAL-X TARGET-RADIUS))
                            TARGET-INITIAL-Y)))
    
    (check-equal?
     (send t-tick-1 target-x)
     TARGET-INITIAL-X
     "Target after tick.")
    
    (check-equal?
     (send t-key-1 target-y)
     TARGET-INITIAL-Y
     "Target after key event.")

    (check-true
     (send t-but-down-1 target-selected?)
     "Target mouse button down inside")

    (check-false
     (send t-but-down-2 target-selected?)
     "Target mouse button down outside")

    (check-false
     (send t-but-up-1 target-selected?)
     "Target mouse button up inside")
    
    (check-false
     (send t-but-up-2 target-selected?)
     "Target mouse button up outside")

    (check-true
     (send t-drag-1 target-selected?)
     "Selected Target mouse drag")
    
    (check-false
     (send t-drag-2 target-selected?)
     "Unselected Target mouse drag")

    (check-equal?
     (send t0 add-to-scene EMPTY-CANVAS)
     (place-image (circle TARGET-RADIUS TARGET-MODE TARGET-COLOR)
                  TARGET-INITIAL-X
                  TARGET-INITIAL-Y
                  EMPTY-CANVAS)
     "Unselected Target to scene")

    (check-equal?
     (send t1 add-to-scene EMPTY-CANVAS)
     (place-image (circle TARGET-RADIUS TARGET-MODE SEL-TARGET-COLOR)
                  TARGET-INITIAL-X
                  TARGET-INITIAL-Y
                  EMPTY-CANVAS)
     "Selecetd Target to scene")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS for Square:

(begin-for-test
  (local
    ((define s0 (make-square-toy TARGET-INITIAL-X TARGET-INITIAL-Y SQUARE-SPEED))
     
     (define s-tick-1 (send s0 after-tick))
     
     (define s-tick-2 (send (new Square%
                                 [x CANVAS-WIDTH]
                                 [y CANVAS-HEIGHT]
                                 [velocity SQUARE-SPEED])
                            after-tick))
     
     (define s-tick-3 (send (new Square%
                                 [x 0]
                                 [y 0]
                                 [velocity (- SQUARE-SPEED)])
                            after-tick))
     
     (define s-tick-4 (send (make-square TARGET-INITIAL-X
                                         TARGET-INITIAL-Y
                                         SQUARE-SPEED
                                         true
                                         TARGET-INITIAL-X
                                         TARGET-INITIAL-Y)
                            after-tick))
     
     (define s-key-1 (send s0 after-key-event NEW-SQUARE-EVENT))
     
     (define s-but-down-1 (send s0 after-button-down TARGET-INITIAL-X TARGET-INITIAL-Y))
     
     (define s-but-down-2 (send s0
                                after-button-down
                                (+ TARGET-INITIAL-X SQUARE-SIDE)
                                TARGET-INITIAL-Y))
     
     (define s-but-up-1 (send s-tick-4 after-button-up TARGET-INITIAL-X TARGET-INITIAL-Y))
     
     (define s-but-up-2 (send s0
                              after-button-up
                              (+ TARGET-INITIAL-X SQUARE-SIDE)
                              TARGET-INITIAL-Y))
     
     (define s-drag-1 (send s-tick-4 after-drag TARGET-INITIAL-X TARGET-INITIAL-Y))
     
     (define s-drag-2 (send s0
                            after-drag
                            (+ TARGET-INITIAL-X SQUARE-SIDE)
                            TARGET-INITIAL-Y)))
    
    (check-equal?
     (send s-tick-1 toy-x)
     (+ TARGET-INITIAL-X SQUARE-SPEED)
     "Unselected Square inside the canvas moves right after tick")
    
    (check-equal?
     (send s-tick-2 toy-x)
     (- CANVAS-WIDTH HALF-SQUARE-SIDE)
     "Unselected Square outside the right-bottom corner after tick")
    
    (check-equal?
     (send s-tick-3 toy-x)
     HALF-SQUARE-SIDE
     "Unselected Square outside the top-left corner after tick")
    
    (check-equal?
     (send s-tick-4 toy-x)
     TARGET-INITIAL-X
     "Selected Square x after tick")

    (check-equal?
     (send s-tick-4 toy-y)
     TARGET-INITIAL-Y
     "Selected Square y after tick")

    (check-equal?
     (send s-tick-4 toy-data)
     SQUARE-SPEED
     "Selected Square velocity after tick")
    
    (check-equal?
     (send s-key-1 toy-x)
     (send s0 toy-x)
     "Square after key event.")
    
    (check-true
     (send s-but-down-1 for-test:selected?)
     "Square mouse button down inside")
    
    (check-false
     (send s-but-down-2 for-test:selected?)
     "Square mouse button down outside")
    
    (check-false
     (send s-but-up-1 for-test:selected?)
     "Square mouse button up inside")
    
    (check-false
     (send s-but-up-2 for-test:selected?)
     "Square mouse button up outside")
    
    (check-true
     (send s-drag-1 for-test:selected?)
     "Selected Square mouse drag")
    
    (check-false
     (send s-drag-2 for-test:selected?)
     "Unselected Square mouse drag")
    
    (check-equal?
     (send s0 add-to-scene EMPTY-CANVAS)
     (place-image (square SQUARE-SIDE SQUARE-MODE SQUARE-COLOR)
                  TARGET-INITIAL-X
                  TARGET-INITIAL-Y
                  EMPTY-CANVAS)
     "Unselected Square to scene")
    
    (check-equal?
     (send s-tick-4 add-to-scene EMPTY-CANVAS)
     (place-image (square SQUARE-SIDE SQUARE-MODE SEL-SQUARE-COLOR)
                  TARGET-INITIAL-X
                  TARGET-INITIAL-Y
                  EMPTY-CANVAS)
     "Selecetd Square to scene")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS for Football:

(begin-for-test
  (local
    ((define f0 (make-football TARGET-INITIAL-X TARGET-INITIAL-Y))
     (define f1 (new Football%
                     [x TARGET-INITIAL-X]
                     [y TARGET-INITIAL-Y]
                     [size 0]
                     [selected? true]
                     [saved-mx TARGET-INITIAL-X]
                     [saved-my TARGET-INITIAL-Y]))
     (define f2 (new Football%
                     [x TARGET-INITIAL-X]
                     [y TARGET-INITIAL-Y]
                     [selected? true]
                     [saved-mx TARGET-INITIAL-X]
                     [saved-my TARGET-INITIAL-Y]))
     
     (define f-tick (send f1 after-tick))    
     (define f-key (send f0 after-key-event NEW-SQUARE-EVENT))
     (define f-but-down (send f0
                              after-button-down
                              (+ TARGET-INITIAL-X CANVAS-WIDTH)
                              TARGET-INITIAL-Y))
     
     (define f-but-up (send f1
                            after-button-up
                            (+ TARGET-INITIAL-X CANVAS-WIDTH)
                            TARGET-INITIAL-Y))
     
     (define f-drag (send f1 after-drag TARGET-INITIAL-X TARGET-INITIAL-Y))
     (define f-drag-1 (send f2 after-drag TARGET-INITIAL-X TARGET-INITIAL-Y)))
    
    (check-equal?
     (send f-tick toy-x)
     TARGET-INITIAL-X
     "Unselected Football after tick")
    
    (check-equal?
     (send f-key toy-y)
     TARGET-INITIAL-Y
     "Unselected Football after key event")
    
    (check-false
     (send f-but-down for-test:selected?)
     "Football mouse button down outside")

    (check-true
     (send f-but-up for-test:selected?)
     "Football mouse button up outside")
    
    (check-true
     (send f-drag for-test:selected?)
     "Selected Football mouse drag")
    
    (check-equal?
     (send f0 add-to-scene EMPTY-CANVAS)
     (place-image (bitmap "football.jpg")
                  TARGET-INITIAL-X
                  TARGET-INITIAL-Y
                  EMPTY-CANVAS)
     "Football to scene")

    (check-equal?
     (send f1 add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS
     "Football to scene with size 0")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS for Clock:

(begin-for-test
  (local
    ((define c0 (make-clock TARGET-INITIAL-X TARGET-INITIAL-Y))
     (define c1 (new Clock%
                     [x TARGET-INITIAL-X]
                     [y TARGET-INITIAL-Y]
                     [selected? true]
                     [saved-mx TARGET-INITIAL-X]
                     [saved-my TARGET-INITIAL-Y]))
     
     (define c-tick (send c1 after-tick))    
     (define c-key (send c0 after-key-event NEW-SQUARE-EVENT))
     (define c-but-down (send c0
                              after-button-down
                              (+ TARGET-INITIAL-X CANVAS-WIDTH)
                              TARGET-INITIAL-Y))
     
     (define c-but-up (send c1
                            after-button-up
                            (+ TARGET-INITIAL-X CANVAS-WIDTH)
                            TARGET-INITIAL-Y))
     
     (define c-drag (send c1 after-drag TARGET-INITIAL-X TARGET-INITIAL-Y)))
    
    (check-equal?
     (send c-tick toy-x)
     TARGET-INITIAL-X
     "Unselected Clock after tick")
    
    (check-equal?
     (send c-key toy-y)
     TARGET-INITIAL-Y
     "Unselected Clock after key event")
    
    (check-false
     (send c-but-down for-test:selected?)
     "Clock mouse button down outside")

    (check-true
     (send c-but-up for-test:selected?)
     "Clock mouse button up outside")
    
    (check-true
     (send c-drag for-test:selected?)
     "Selected Clock mouse drag")
    
    (check-equal?
     (send c0 add-to-scene EMPTY-CANVAS)
     (place-image
      (text (number->string 0)
            TIMER-TEXT-SIZE BLACK)
      TARGET-INITIAL-X TARGET-INITIAL-Y
      (scene+line
       (place-image 
        (foldr
         (lambda (elt d)
           (rotate
            QUARTER-DEGREES-IN-CIRCLE
            (overlay/align CENTER TOP (text (number->string elt) DIAL-TEXT BLACK) d)))
         (circle CLOCK-RADIUS CLOCK-CIRCLE-TYPE BLACK)
         DIGITS-LIST) TARGET-INITIAL-X TARGET-INITIAL-Y EMPTY-CANVAS)
       TARGET-INITIAL-X TARGET-INITIAL-Y TARGET-INITIAL-X (- TARGET-INITIAL-Y HAND-RADIUS)
       RED))
     "Clock to scene")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS for Throbber:

(begin-for-test
  (local
    ((define t0 (make-throbber TARGET-INITIAL-X TARGET-INITIAL-Y))
     (define t-tick-1 (send t0 after-tick))
     (define t-tick-2 (send (new Throbber%
                           [x (send t0 toy-x)]
                           [y (send t0 toy-y)]
                           [r (sub1 THROBBER-MAX-RADIUS)])
                      after-tick))
     (define t-tick-3 (send (new Throbber%
                           [x (send t0 toy-x)]
                           [y (send t0 toy-y)]
                           [r (add1 THROBBER-MIN-RADIUS)]
                           [operator -])
                      after-tick))
     (define t-tick-4 (send (new Throbber%
                           [x (send t0 toy-x)]
                           [y (send t0 toy-y)]
                           [selected? true]
                           [saved-mx (send t0 toy-x)]
                           [saved-my (send t0 toy-y)])
                      after-tick))
     (define t-key-1 (send t0 after-key-event NEW-THROBBER-EVENT))

     (define t-but-down-1 (send t0 after-button-down TARGET-INITIAL-X TARGET-INITIAL-Y))

     (define t-but-down-2 (send t0
                                after-button-down
                                (add1 (+ TARGET-INITIAL-X (send t0 toy-data)))
                                TARGET-INITIAL-Y))
     
     (define t-but-up-1 (send t-tick-4 after-button-up TARGET-INITIAL-X TARGET-INITIAL-Y))
     
     (define t-but-up-2 (send t0
                              after-button-up
                              (add1 (+ TARGET-INITIAL-X (send t-tick-4 toy-data)))
                              TARGET-INITIAL-Y))

     (define t-drag-1 (send t-tick-4 after-drag TARGET-INITIAL-X TARGET-INITIAL-Y))
     
     (define t-drag-2 (send t0
                            after-drag
                            (add1 (+ TARGET-INITIAL-X (send t0 toy-data)))
                            TARGET-INITIAL-Y)))
    
    (check-equal?
     (send t-tick-1 toy-data)
     (+ THROBBER-EXP-CON-RATE THROBBER-RADIUS)
     "Unselected Throbber after one tick.")
    
    (check-equal?
     (send t-tick-2 toy-data)
     THROBBER-MAX-RADIUS
     "Unselected Throbber after one tick reaches to maximum radius.")
    
    (check-equal?
     (send t-tick-3 toy-data)
     THROBBER-MIN-RADIUS
     "Unselected Throbber after one tick reaches to minimum radius.")
    
    (check-equal?
     (send t-tick-4 toy-data)
     (send t0 toy-data)
     "Selected Throbber after one tick.")
    
    (check-equal?
     (send t-key-1 toy-data)
     (send t0 toy-data)
     "Throbber after key event.")

    (check-true
     (send t-but-down-1 for-test:selected?)
     "Throbber mouse button down inside")

    (check-false
     (send t-but-down-2 for-test:selected?)
     "Throbber mouse button down outside")

    (check-false
     (send t-but-up-1 for-test:selected?)
     "Throbber mouse button up inside")
    
    (check-false
     (send t-but-up-2 for-test:selected?)
     "Throbber mouse button up outside")

    (check-true
     (send t-drag-1 for-test:selected?)
     "Selected Throbber mouse drag")
    
    (check-false
     (send t-drag-2 for-test:selected?)
     "Unselected Throbber mouse drag")

    (check-equal?
     (send t0 add-to-scene EMPTY-CANVAS)
     (place-image (circle THROBBER-RADIUS THROBBER-MODE THROBBER-COLOR)
                  TARGET-INITIAL-X
                  TARGET-INITIAL-Y
                  EMPTY-CANVAS)
     "Unselected Throbber to scene")

    (check-equal?
     (send t-tick-4 add-to-scene EMPTY-CANVAS)
     (place-image (circle THROBBER-RADIUS THROBBER-MODE SEL-THROBBER-COLOR)
                  TARGET-INITIAL-X
                  TARGET-INITIAL-Y
                  EMPTY-CANVAS)
     "Selecetd Throbber to scene")))