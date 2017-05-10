#lang racket
;; Blocks.rkt : Class definition for Block class.

;; Require statements:
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "sets.rkt")
(require "extras.rkt")
(require "interfaces.rkt")

;; Provide statements:
(provide Block%)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS

;; A ListOfBlock<%> is one of
;; -- empty
;;    INTERP: A sequence of Block<%> with no elements
;; -- (cons Block<%> ListOfBlock<%>)
;;    INTERP: (cons b lst) represents the sequence of Blocks<%> whose
;;    first element is b and whose other elements are represented by lst

;; TEMPLATE:
;; lob-fn : ListOfBlock<%> -> ??
#|
(define (lob-fn lob)
  (cond
    [(empty? lob) ...]
    [else (...
           (block-fn (first lob))
           (lob-fn (rest lob)))]))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; A Block is a (new Block% [x Int][y Int][other-blocks ListOfBlock<%>])
;; A Block represents a block which lives in block factory.
(define Block%
  (class* object% (Block<%>)

    ;; initial values of x, y (center of blocks)
    (init-field x)
    (init-field y)

    ;; list of blocks present in world other than this one
    (init-field other-blocks)
    
    ;; is this selected? Default is false.
    (init-field [selected? false]) 

    ;; if this is selected, the position of the last button-down event inside this,
    ;; relative to the block's center. Else any value.
    (init-field [saved-mx 0] [saved-my 0]) 

    ;; the team mates of current block
    (field [team-mates empty])

    ;; the measurement of block's side
    (field [SIDE 20])

    ;; half the measurement of block's side
    (field [HALF-SIDE (/ SIDE 2)])

    ;; image for displaying unselected square
    (field [SQUARE-IMG (square SIDE "outline" "green")])

    ;; image for displaying selected square
    (field [SEL-SQUARE-IMG (square SIDE "outline" "red")])

    ;; update-blocks-in-factory: Block<%> -> Void
    ;; GIVEN: a new block
    ;; EFFECT: adds given block to other-blocks field
    ;;         in blocks present already in factory.
    ;; Examples: See tests below
    ;; STRATEGY: Use HOF map on blocks-list 
    (define (update-blocks-in-factory block)
      (map
       ;; Block<%> -> Void
       ;; EFFECT: updates <TODO>
       (lambda (obj)
             (send obj update-other-blocks block))
           other-blocks))
    
    ;; registers this block in other blocks lists
    (field [register-blocks (update-blocks-in-factory this)])
    
    (super-new)

    
    ;; update-other-blocks: Block<%> -> Void
    ;; GIVEN: a Block<%> to be added to other blocks
    ;; EFFECT: adds the given block to other blocks field of this block
    ;; STRATEGY: Divide into cases whether object present in other-blocks list
    ;; EXAMPLES: see tests below
    (define/public (update-other-blocks obj)
      (if (not (member obj other-blocks)) 
        (set! other-blocks (cons obj other-blocks))
        this))

    
    ;; after-tick : -> Block<%>
    ;; GIVEN: no arguments
    ;; RETURNS: the same object which it is working on Block<%>
    ;; DETAILS: a Block ignores key events
    ;; EXAMPLES: see tests below
    (define/public (after-tick) this)

    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a scene like given one but with the block painted on it
    ;; EXAMPLES: see tests below
    ;; STRATEGY: combine simpler functions
    (define/public (add-to-scene s)
      (place-image (if selected? SEL-SQUARE-IMG SQUARE-IMG)
                   x
                   y
                   s))

    
    ;; after-button-down : Integer Integer -> Void
    ;; GIVEN: the x,y location of a button-down event
    ;; EFFECT: same object if the mouesclick is outside it, if it is inside
    ;;          returns a selected object with saved-mx,saved-my updated.
    ;; Example: Let block A centered at 100,100 get a mouse click at 101,101.
    ;;          The function will make block A as selected and update it's relative
    ;;          co-ordinates
    ;; STRATEGY: Cases on whether the event is in this
    (define/public (after-button-down mx my)
      (if (in-square? mx my)
        (begin
          (set! selected? true)
          (set! saved-mx (- mx x))
          (set! saved-my (- my y)))
        this))

    
    ;; after-button-up : Integer Integer -> Void
    ;; GIVEN: the location of a button-up event
    ;; EFFECT: same object if the mouesclick is outside it, if it is inside
    ;;          returns a selected object with saved-mx,saved-my updated.
    ;; Example: Let block A centered at 100,100 get a mouse up at 101,101.
    ;;          The function will make block A as unselected and update it's relative
    ;;          co-ordinates
    ;; STRATEGY: Combine simpler functions
    (define/public (after-button-up mx my)
      (set! selected? false))
        

    ;; after-drag : Integer Integer -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT:  If block is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my). And add overlapping blocks to teammates list
    ;; Example: Let block A situated at 100,100 be dragged to block B sitauted at 130,130
    ;;          the function will update A's team mates list to include B and vice versa
    ;; STRATEGY: Cases on whether the block is selected.
    (define/public (after-drag mx my)
      (if selected?
        (begin
          (move-teammates mx my)
          (set! x (- mx saved-mx))
          (set! y (- my saved-my))
          (add-overlapping-blocks))
          this))

    
    ;; move-teammates: Nat Nat -> Void
    ;; GIVEN: a location of mouse click
    ;; EFFECT: moves all team mates of current block by appropraite distance
    ;; Example: Let block A have team mates block B, the function will move both A,B
    ;;          by appropraite distances.
    ;; STRATEGY: Use HOF map on team-mates
    (define (move-teammates mx my)
      (map
       ;; Block<%> -> Void
       ;; EFFECTS: moves the given block with a distance calculated by getting difference
       ;;          of mouse co-ordinates , relative mouse distance, and x,y co-ordiantes
       (lambda (obj)
         (send obj move-by-distance (- mx saved-mx x) (- my saved-my y)))
       team-mates))

    
    ;; move-by-distance: Int Int -> Void
    ;; GIVEN: distances along x and y axis
    ;; EFFECT: Updates the x and y co-ordinates so that the block moves by given distance
    ;; Exmaple: Let block A be placed at 100,100 and the dist-x and dist-y be 1,1
    ;;          the function will move the block to 101,101
    ;; STRATEGY: Combine simpler functions
    (define/public (move-by-distance dist-x dist-y)
      (set! x (+ dist-x x))
      (set! y (+ dist-y y)))

    
    ;; check-x-limit: Int Int -> Boolean
    ;; GIVEN: two x co-ordinates
    ;; RETURNS: true iff the given co-ordinates are at a distance of SIDE apart
    ;;          from each other
    ;; Example: (check-x-limit 100 101) = #t
    ;; STRATEGY: Combine simpler functions
    (define (check-x-limit x1 x2)
      (and (<= (- x1 x2) SIDE) (>= (- x1 x2) (- SIDE))))

    
    ;; check-y-limit: Int Int -> Boolean
    ;; GIVEN: two y co-ordinates
    ;; RETURNS: true iff the given co-ordinates are at a distance of SIDE apart
    ;;          from each other
    ;; Example: (check-y-limit 100 101) = #t
    ;; STRATEGY: Combine simpler functions
    (define (check-y-limit y1 y2)
      (and (<= (- y1 y2) SIDE) (>= (- y1 y2) (- SIDE))))

    
    ;; add-overlapping-blocks: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: adds all overlapping blocks and their team mates to
    ;;         current blocks team mates
    ;; Example: Let Block C be overlapping with current block A, function will add
    ;;          C to the team mates list of A
    ;; STRATEGY: Use HOF map on result of function (overlap-blocks)
    (define (add-overlapping-blocks)
      (map
       ;; Block<%> -> Void
       ;; EFFECT: adds given block and it's team to this block's team mates
       (lambda (obj)
         (add-teammate obj))
        (overlap-blocks)))

    
    ;; overlap-blocks: -> ListOfBlock<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a list of Block<%> which are non-team-members and are overlapping
    ;;          the current block
    ;; Exmaple: Let block A have team mates (list B), and the blocks present in the world
    ;;          be (list B C). Now if A,C are intersecting then function
    ;;          will return (list C)
    ;; STRATEGY: Use HOF filter on result of non-team-member
    (define (overlap-blocks)
      (filter
       ;; Block<%> -> Boolean
       ;; RETURNS: true if the given block overlaps this block
       (lambda (obj)
           (and (check-x-limit (send obj block-x) x)
                (check-y-limit (send obj block-y) y)))
         (non-team-member)))
    

    ;; non-team-member: -> ListOfBlock<%>
    ;; GIVEN: no arguments
    ;; RETURNS: list of blocks which are present in the world but are not team members of
    ;;         this block
    ;; Exmaple: Let block A have team mates (list B), and the blocks present in the world
    ;;          be (list B C). the function should return (list C)
    ;; STRATEGY: Combine simpler functions
    (define (non-team-member)
        (set-subtract other-blocks team-mates (list this)))

    
    ;; add-teamate: Block<%> -> Void
    ;; GIVEN: a Block<%> to be added to team
    ;; EFFECTS: joins team members of new block with current one, and updates all of them
    ;;          with new team
    ;; Examples: Let block A with no team mates be passed block b with no team mates
    ;;           function will update block A's team mates list as (list B)
    ;; STRATEGY: USE HOF map on result of (get-new-team newblock)
    (define/public (add-teammate newblock)
      (local
        ((define new-team (get-new-team newblock)))
      (map
       ;; Block<%> -> Void
       ;; EFFECT: updates the team mates of this block and given block
       ;;         with new team mates list
       (lambda (obj)
           (send obj set-team-mates-without-self new-team))
       new-team)))

    
    ;; get-new-team: Block<%> -> ListOfBlock<%>
    ;; GIVEN: a Block<%>
    ;; RETURNS: a list of blocks which is combination of current blocks team members
    ;;          and given blocks team members
    ;; Examples: Let the block A be passed to block B. function will return a union of A,B
    ;;           and their team mates
    ;; STRATEGY: Combine simpler functions
    (define (get-new-team block)
      (append (send block get-team) (cons block (cons this team-mates))))

    
    ;; set-team-mates-without-self: ListOfBlocks<%> -> Void
    ;; GIVEN: a List of Blocks<%>
    ;; EFFECT: Updates the team mates of current block with given list,
    ;;         filtering itself out
    ;; Examples: Let the list passed to block A be A,B,C. A will update the team mates
    ;;           with list B,C
    ;; STRATEGY: Use HOF filter on lst
    (define/public (set-team-mates-without-self lst)
      (set! team-mates (set-subtract lst (list this))))

    
    ;; get-team: -> ListOfBlock<%>
    ;; GIVEN: no arguments
    ;; RETURNS: the team mates of current block
    ;; EXAMPLES: see tests below
    (define/public (get-team) team-mates)

    
    ;; block-x: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: x coordinate of center of the block
    ;; EXAMPLES: see tests below 
    (define/public (block-x) x)

    
    ;; block-y: -> Integer
    ;; GIVEN: no arguments
    ;; RETURNS: y coordinate of center of the block
    ;; EXAMPLES: see tests below
    (define/public (block-y) y)

    
    ;; after-key-event: KeyEvent -> Block<%>
    ;; GIVEN: a Key Event kev
    ;; RETURNS: the current object implementing block interface
    ;; DETAILS: the block ignores key events
    (define/public (after-key-event kev) this)

    
    ;; in-square?: Integer Integer -> Boolean
    ;; GIVEN: a location on the canvas
    ;; RETURNS: true iff the location is inside this square.
    ;; STRATEGY: Combine simpler functions
    (define (in-square? other-x other-y)
      (and (<= (- x HALF-SIDE) other-x (+ x HALF-SIDE))
           (<= (- y HALF-SIDE) other-y (+ y HALF-SIDE))))

    
    ;; -> Boolean
    ;; Returns: whether block is selected
    (define/public (for-test:selected?) selected?)

    
    ;; Methods for Testing
    ;; -> ListOfBlock<%>
    ;; Returns: list of blocks present in the world
    (define/public (for-test:other-blocks) other-blocks)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS FOR TESTING:

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define SIDE 20)
(define NEW-BLOCK-EVENT "b")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define b0 (new Block% [x 100] [y 100] [other-blocks empty]))
     (define b1 (new Block% [x 150] [y 150] [other-blocks (list b0)]))
     (define b2 (new Block% [x 30] [y 30] [other-blocks empty]))
     (define b3 (new Block% [x 40] [y 40] [other-blocks empty]))
     (define b4 (new Block% [x 60] [y 60] [other-blocks empty])))

    (send b0 after-tick)

    (check-equal?
     (send b0 block-x)
     100
     "After tick no change in x coordinate")

    (send b0 after-key-event NEW-BLOCK-EVENT)

    (check-equal?
     (send b0 block-y)
     100
     "After key event no change in y coordinate")

    (check-equal?
     (send b0 add-to-scene EMPTY-CANVAS)
     (place-image  (square SIDE "outline" "green") 100 100 EMPTY-CANVAS)
     "Image of unselected block should be displayed")

    (send b0 after-button-down 50 50)
    (check-false
     (send b0 for-test:selected?)
     "unselected block after button down mouse event not inside it")

    (send b0 after-button-down 100 100)
    (check-true
     (send b0 for-test:selected?)
     "selected after button down mouse event")

    (check-equal?
     (send b0 add-to-scene EMPTY-CANVAS)
     (place-image  (square SIDE "outline" "red") 100 100 EMPTY-CANVAS)
     "Image of selected block")

     (send b0 after-button-up 100 100)
    (check-false
     (send b0 for-test:selected?)
     "Unselected block after button up")

    (check-equal?
     (send b0 after-drag 100 100)
     b0
     "no change for unselected drag blocks")
    
    (send b0 update-other-blocks b1)
    (check-equal?
     (send b0 for-test:other-blocks)
     (list b1)
     "updated list of other blocks")

    (send b0 after-button-down 100 100)
    (send b0 after-drag 130 130)
    (send b0 after-drag 130 130)
    (check-equal?
     (send b0 get-team)
     (list b1)
     "team members updated on overlap of blocks")
    
    (send b2 add-teammate b3)
    (send b2 add-teammate b4)
    (send b2 update-other-blocks b0)
    (send b3 update-other-blocks b0)
    (send b4 update-other-blocks b0)
    (send b0 update-other-blocks b2)
    (send b0 update-other-blocks b3)
    (send b0 update-other-blocks b4)
    (send b0 after-drag 50 50)
     (set-equal?
     (send b0 get-team)
     (list b4 b3 b2 b1))))