;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; robot.rkt: Implements some behavioral of bishop in the chessboard.
;; In an infinite chessboard, we have a robot and some blocks. The robot occupies
;; a single square on the chessboard, as does each of the blocks. The robot can move
;; any number of squares in any diagonal direction, but it can never move to or through
;; a square occupied by a block.

;; path will give a plan that, when executed, will take the robot from the starting
;; position to the target position without passing over any of the blocks, or false
;; if no such sequence of moves exists.

;; eval-plan will give the position of the robot at the end of executing the plan,
;; or false if the plan sends the robot to or through any block.

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(provide
 path
 eval-plan)

;; check the location of file for automated testing
;; (check-location "08" "robot.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS

;; for directions
(define NE "ne")
(define SE "se")
(define SW "sw")
(define NW "nw")

;; for one step movement
(define ONE-STEP 1)
(define INIT-VAL 0)

;; for creating the virtual boundary of the chessboard
(define EXTRA-SPACE-IN-BORDER 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

;; A Position is a (list Integer Integer)
;; (list x y) represents the position (x,y) in cartesian coordinate system.

;; TEMPLATE:
;; pos-fn : Position -> ??
#|
(define (pos-fn pos)
  (...
   (first pos)
   (second pos)))
|#


;; A ListOfPosition is one of
;; -- empty
;; -- (cons Position ListOfPosition)

;; TEMPLATE:
;; lop-fn : ListOfPosition -> ??
#|
(define (lop-fn lop)
  (cond
    [(empty? lop) ...]
    [else (... (pos-fn (first lop))
               (lop-fn (rest lop)))]))
|#

;; examples of ListOfPosition
(define BLOCKS-1 '((0 3) (2 3) (4 3) (0 5) (4 5) (0 7) (2 7) (4 7)))
(define BLOCKS-2 '((0 3) (4 3) (0 5) (4 5) (0 7) (4 7) (0 9) (4 9) (0 11) (4 11)))

;; A Direction is one of
;; -- NE
;; -- SE
;; -- SW
;; -- NW
;; INTERPRETATION: direction in which robot will move.

;; TEMPLATE:
;; dir-fn : Direction -> ??
#|
(define (dir-fn d)
  (cond
    [(string=? d NE) ...]
    [(string=? d SE) ...]
    [(string=? d SW) ...]
    [(string=? d NW) ...]))
|#


;; A Move is a (list Direction PosInt)
;; INTERPRETATION: a move of the specified number of steps in the indicated
;; direction.

;; TEMPLATE:
;; move-fn : Move -> ??
#|
(define (move-fn m)
  (...
   (dir-fn (first m))
   (second m)))
|#


;; A ListOfMove is one of
;; -- empty
;; -- (cons Move ListOfMove)

;; TEMPLATE:
;; lom-fn : ListOfMove -> ??
#|
(define (lom-fn lom)
  (cond
    [(empty? lom) ...]
    [else (... (move-fn (first lom))
               (lom-fn (rest lom)))]))
|#


;; A Plan is a ListOfMove
;; WHERE: the list does not contain two consecutive moves in the same
;; direction.
;; INTERP: the moves are to be executed from the first in the list to
;; the last in the list.


;; A MaybeX is one of
;; -- false
;; -- X

;; TEMPLATE:
;; maybex-fn : MaybeX -> ??
#|
(define (maybex-fn mx)
  (cond
    [(false? mx) ...]
    [else (... mx)]))
|#

(define-struct border (minx miny maxx maxy))
;; A Boundary is a (make-border Integer Integer Integer Integer)
;; INTERPRETATION:
;; minx, miny, maxx and maxy represents the left, bottom, right and top boundary
;; of the chessboard respectively.

;; border-fn : Boundary -> ??
#|
(define (border-fn b)
  (...
    (border-minx b)
    (border-miny b)
    (border-maxx b)
    (border-maxy b)))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; functions for interacting with data

;; pos-x : Position -> Integer
;; RETURNS: the x-coordinate of given position.
;; EXAMPLES: (pos-x (list 1 2)) = 1
;; DESIGN STRATEGY: Use template for Position on pos
(define (pos-x pos)
  (first pos))

;; pos-y : Position -> Integer
;; RETURNS: the y-coordinate of given position.
;; EXAMPLES: (pos-y (list 1 2)) = 2
;; DESIGN STRATEGY: Use template for Position on pos
(define (pos-y pos)
  (second pos))

;; make-pos : Integer Integer -> Position
;; RETURNS: the position at given x- and y-coordinates
;; EXAMPLES: (make-pos 1 2) = (list 1 2)
;; DESIGN STRATEGY: Combine simpler function
(define (make-pos x y)
  (list x y))

;; move-dir : Move -> Direction
;; RETURNS: the direction of the given move
;; EXAMPLES: (move-dir (list NE 2)) = NE
;; DESIGN STRATEGY: Use template for Move on m
(define (move-dir m)
  (first m))

;; move-steps : Move -> PosInt
;; RETURNS: the number of steps in given move
;; EXAMPLES: (move-steps (list NE 2)) = 2
;; DESIGN STRATEGY: Use template for Move on m
(define (move-steps m)
  (second m))

;; make-move : Integer Integer -> Position
;; RETURNS: the position at given x- and y-coordinates
;; EXAMPLES: (make-move 1 2) = (list 1 2)
;; DESIGN STRATEGY: Combine simpler function
(define (make-move dir steps)
  (list dir steps))

;; move-in-same-dir? : Move Move -> Boolean
;; RETURNS: true if the direction of the given moves are same
;; EXAMPLES: (move-in-same-dir? (list NE 2) (list NE 4)) = true
;; DESIGN STRATEGY: Combine simpler function
(define (move-in-same-dir? m1 m2)
  (string=? (move-dir m1) (move-dir m2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; path : Position Position ListOfPosition -> MaybePlan
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. the target position that robot is supposed to reach
;; 3. A list of the blocks on the board
;; RETURNS: a plan that, when executed, will take the robot from  the starting
;; position to the target position without passing over any of the blocks, or
;; false if no such sequence of moves exists.
;; EXAMPLES:
;; (path (list 2 5) (list 4 9) BLOCKS-1) = false
;; (path (list 2 5) (list 5 8) empty) = (list (list NE 3))
;; DESIGN STRATEGY: Cases on weather start and targt positions are covered by blocks
;;                  or not and they are in same wing or not
(define (path strt targt blocks)
  (cond
    [(position-covered-by-blocks? strt blocks) false]
    [(position-covered-by-blocks? targt blocks) false]
    [(not (positions-on-same-wing? strt targt)) false]
    [else (move-towards-target strt targt blocks empty empty
                               (chessboard-boundary (append (list strt targt) blocks)))]))


;; positions-on-same-wing? : Position Position -> Boolean
;; GIVEN: the two positions
;; RETURNS: weather both the positions are in same wing or not. Wing is like white
;; or black squares in chessboard.
;; EXAMPLES:
;; (positions-on-same-wing? (list 1 1) (list 2 2)) = true
;; (positions-on-same-wing? (list 1 1) (list 2 3)) = false
;; DESIGN STRATEGY: Combine simpler functions
(define (positions-on-same-wing? p1 p2)
  (or (and (odd? (+ (pos-x p1) (pos-y p1)))
           (odd? (+ (pos-x p2) (pos-y p2))))
      (and (even? (+ (pos-x p1) (pos-y p1)))
           (even? (+ (pos-x p2) (pos-y p2))))))


;; position-covered-by-blocks? : Position ListOfPosition -> Boolean
;; GIVEN: a position and a list of blocks on the board
;; RETURNS: true if the given position is covered by the blocks
;; EXAMPLES:
#|
(position-covered-by-blocks? (list 1 2) (list (list 2 3) (list 2 1) (list 0 3)))
  = false
|#
;; DESIGN STRATEGY: Combine simpler functions
(define (position-covered-by-blocks? pos blocks)
  (or (my-member? pos blocks)
      (empty? (set-diff (all-next-posns pos) blocks))))


;; all-next-posns : Position -> ListOfPosition
;; GIVEN: the current position
;; RETURNS: a list of all four possible positions after one step movement from the
;; given position
;; EXAMPLES:
#|
(all-next-posns (list 1 2))
  = (list (list 2 3) (list 2 1) (list 0 3) (list 0 1))
|#
;; DESIGN STRATEGY: Combine simpler functions
(define (all-next-posns pos)
  (list (make-pos (add1 (pos-x pos)) (add1 (pos-y pos)))
        (make-pos (add1 (pos-x pos)) (sub1 (pos-y pos)))
        (make-pos (sub1 (pos-x pos)) (add1 (pos-y pos)))
        (make-pos (sub1 (pos-x pos)) (sub1 (pos-y pos)))))


;; chessboard-boundary : ListOfPosition -> Boundary
;; GIVEN: a list of all the positions in the board
;; RETURNS: the boundary of the board such that all the positions comes inside the
;;          boundary and have some extra space for the movement of robot.
;; EXAMPLES:
#|
(chessboard-boundary (list (list -3 3) (list -4 -2) (list 2 -2) (list 3 2)))
  = (make-border -6 -4 5 5)
|#
;; DESIGN STRATEGY: Use HOF foldr on lop
(define (chessboard-boundary lop)
  (border-after-adding-extra-space 
   (foldr
    ;; Position Boundary -> Boundary
    ;; GIVEN: the position and a boundary
    ;; RETURNS: the boundary after increasing such that it covers the given position
    (lambda (pos brdr)
      (make-border (min (pos-x pos) (border-minx brdr))
                   (min (pos-y pos) (border-miny brdr))
                   (max (pos-x pos) (border-maxx brdr))
                   (max (pos-y pos) (border-maxy brdr))))
    (make-border INIT-VAL INIT-VAL INIT-VAL INIT-VAL)
    lop)
   EXTRA-SPACE-IN-BORDER))


;; border-after-adding-extra-space : Boundary Integer -> Boundary
;; GIVEN: the boundary and an integer value
;; RETURNS: the boundary after extending its area in all directions by given space
;; EXAMPLES:
#|
(border-after-adding-extra-space (make-border -6 -4 5 7)) = (make-border -8 -6 7 9)
|#
;; DESIGN STRATEGY: Combine simpler functions
(define (border-after-adding-extra-space brdr space)
  (make-border (- (border-minx brdr) space)
               (- (border-miny brdr) space)
               (+ (border-maxx brdr) space)
               (+ (border-maxy brdr) space)))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; move-towards-target : Position Position ListOfPosition
;;                       ListOfPosition ListOfMove Boundary -> MayBePlan
;; GIVEN: a current and target position of the robot, a list of the blocks on the board,
;;        a list of positions already visitied by robot, list of moves and a boundary
;; WHERE: lovp contains all the positions visited by the robot from its starting position 
;;        to current position,
;;        lom contains all the moves whose execution on the starting position will take
;;        us the current position. 
;; RETURNS: a plan that, when executed, will take the robot from  the starting position
;;          to the target position without passing over any of the blocks, or false if
;;          no such sequence of moves exists.
;; EXAMPLES:
#|
(move-towards-target (list 2 5) (list 4 9) BLOCKS-1 empty empty (make-border -2 1 6 11))
   = false
(move-towards-target (list 2 5) (list 5 8) empty empty empty (make-border 0 3 7 10))
   = (list (list NE 3))
|#
;; DESIGN STRATEGY: Recur on next position 
;; HALTING MEASURE: ((No of squares covered by the boundary/2) - no of squares covered by
;;                  blocks - no of squares already visited)
;; TERMINATION ARGUMENT: At the recursive call, robot moves to next step and thus
;; no of visited squares increased by one so our haulting measure is decreased by
;; one and if robot does not reache to target position through visiting all the
;; available squares, program terminates by returning false.
(define (move-towards-target curr targt blocks lovp lom brdr)
  (cond
    [(equal? curr targt) (reverse lom)]
    [else
      (local
        ((define next (next-posn curr targt blocks lovp brdr)))
        (cond
          [(false? next) false]
          [else (move-towards-target next targt blocks (cons curr lovp)
                                     (moves-after-adding-move curr next lom) brdr)]))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; next-posn : Position Position ListOfPosition ListOfPosition Boundary -> MayBePosition
;; GIVEN: the current and destination position, a list of the blocks on the board,
;;        a list of previously visited positions and a boundary
;; RETURNS: the next position if movement is possible else returns false.
;; EXAMPLES: (next-posn (list 2 5) (list 4 9) BLOCKS-1) = false
;; DESIGN STRATEGY: Use template for ListOfPosition on lopp
(define (next-posn curr targt blocks lovp brdr)
  (local
    ((define lopp (possible-next-posns curr blocks brdr)))
    (cond 
      [(empty? lopp) false]
      [else (if (empty? (rest lopp))
                (first lopp)
                (next-posn-from-posns (set-diff lopp lovp) targt))])))


;; possible-next-posns : Position ListOfPosition Boundary -> ListOfPosition
;; GIVEN: a current position of robot, a list of blocks on the board and
;;        a boundary
;; RETURNS: the list of possible next positions from the current position
;;          for one step movement
;; EXAMPLES:
#|
(possible-next-posns (list 2 5) empty (make-border 0 3 4 6))
  = (list (list 3 6) (list 3 4) (list 1 6) (list 1 4))
|#
;; DESIGN STRATEGY: Use HOF filter on (all-next-posns curr)
(define (possible-next-posns curr blocks brdr)
  (filter
   ;; Position -> Boolean
   ;; GIVEN: one of the position
   ;; RETURNS: position is able to become next position or not
   (lambda (pos)
     (and (not (my-member? pos blocks))
          (in-border? pos brdr)
          (not (empty? (set-diff (set-diff (all-next-posns pos) (list curr)) blocks)))))
   (all-next-posns curr)))

;; in-border? : Position Boundary -> Boolean
;; GIVEN: a position of robot on board and a boundary
;; RETURNS: weather the given position is inside the boundary or not
;; EXAMPLES: (in-border? (list 1 2) (make-border 0 1 2 3)) = true
;; DESIGN STRATEGY: Combine simpler functions
(define (in-border? pos brdr)
  (and (>= (pos-x pos) (border-minx brdr))
       (>= (pos-y pos) (border-miny brdr))
       (<= (pos-x pos) (border-maxx brdr))
       (<= (pos-y pos) (border-maxy brdr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; next-posn-from-posns : ListOfPosition Position -> MayBePosition
;; GIVEN: a list of possible next positions and a destination position
;; RETURNS: false if the given list is empty else returns the nearest position to
;; the destination 
;; EXAMPLES: (next-posn-from-posns empty (list 2 5)) = false
;; DESIGN STRATEGY: Use template for ListofPosition on lorp
(define (next-posn-from-posns lorp targt)
  (cond
    [(empty? lorp) false]
    [else (if (empty? (rest lorp))
              (first lorp)
              (nearest-posn-to-target lorp targt))]))


;; nearest-posn-to-target : ListOfPosition Position -> Position
;; GIVEN: a list of possible next positions and a destination position
;; WHERE: a list contains more than one position
;; RETURNS: the nearest next position to the destination 
;; EXAMPLES: (nearest-posn-to-target (list (list 1 6) (-1 4)) (list 3 8)) = (list 1 6)
;; DESIGN STRATEGY: Use HOF foldr on (rest lop)
(define (nearest-posn-to-target lop targt)
  (foldr
   ;; Position Position -> Position
   ;; GIVEN: two positions of the robot
   ;; RETURNS: the nearest next position to the destination 
   (lambda (pos nearest-posn)
     (if (< (distance-between-posns pos targt)
            (distance-between-posns nearest-posn targt))
         pos
         nearest-posn))
   (first lop)
   (rest lop)))

;; distance-between-posns : Position Position -> NonNegReal
;; GIVEN: two positions
;; RETURNS: the distance between two positions using formula
;;          (square root of ((x2 - x1)^2 + (y2 - y1)^2))
;; EXAMPLES: (distance-between-points (list 0 0) (list 3 0)) = 3
;; DESIGN STRATEGY: Combine simpler functions
(define (distance-between-posns p1 p2)
  (sqrt (+ (sqr (- (pos-x p2) (pos-x p1)))
           (sqr (- (pos-y p2) (pos-y p1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; moves-after-adding-move : Position Position ListOfMove -> ListOfMove
;; GIVEN: current and next position of the robot and list of moves
;; RETURNS: the list of moves after adding the move obtained from current and next
;; position
;; EXAMPLES:
;; (moves-after-adding-move (list 2 3) (list 3 2) empty) = (list (list SE 1))
;; DESIGN STRATEGY: Use template for ListOfMove on lom and then cases on weather the
;;                  moves are in same direction or not
(define (moves-after-adding-move curr next lom)
  (local
    ((define move (make-move (dir-from-curr-and-next-pos curr next) ONE-STEP)))
    (cond
      [(empty? lom) (cons move lom)]
      [else (if (move-in-same-dir? move (first lom))
                (cons (make-move (move-dir (first lom)) (add1 (move-steps (first lom))))
                      (rest lom))
                (cons move lom))])))

;; dir-from-curr-and-next-pos : Position Position -> Direction
;; GIVEN: current and next position of the robot
;; RETURNS: the direction of the movement
;; EXAMPLES: (dir-from-curr-and-next-pos (list 2 3) (list 3 2)) = SE
;; DESIGN STRATEGY: Combine simpler functions
(define (dir-from-curr-and-next-pos curr next)
  (cond
    [(and (< (pos-x next) (pos-x curr))
          (< (pos-y next) (pos-y curr))) SW]
    [(and (< (pos-x next) (pos-x curr))
          (> (pos-y next) (pos-y curr))) NW]
    [(and (> (pos-x next) (pos-x curr))
          (< (pos-y next) (pos-y curr))) SE]
    [(and (> (pos-x next) (pos-x curr))
          (> (pos-y next) (pos-y curr))) NE]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; eval-plan : Position ListOfPosition Plan ->  MaybePosition
;; GIVEN:
;; 1. the starting position of the robot,
;; 2. A list of the blocks on the board
;; 3. A plan for the robot's motion
;; RETURNS:
;; The position of the robot at the end of executing the plan, or false
;; if  the plan sends the robot to or  through any block.
;; EXAMPLES:
#|
(eval-plan (list 2 5) (rest BLOCKS-1) (path (list 2 5) (list 4 9) (rest BLOCKS-1)))
   = (list 4 9)
|#
;; DESIGN STRATEGY: call a more general function
(define (eval-plan strt blocks lom)
  (execute-plan strt blocks lom))


;; execute-plan : Position ListOfPosition Plan ->  MaybePosition
;; GIVEN: the current position of the robot, a list of the blocks on the board
;;        and a plan for the robot's motion
;; RETURNS: The position of the robot at the end of executing the plan, or false
;;          if  the plan sends the robot to or through any block.
;; EXAMPLES:
#|
(execute-plan (list 2 5) (rest BLOCKS-1) (path (list 2 5) (list 4 9) (rest BLOCKS-1)))
   = (list 4 9)
|#
;; DESIGN STRATEGY: Recur on list of moves in the plan lom
;; HALTING MEASURE: the sum of steps ofeach move of lom
;; TERMINATION ARGUMENT: At the recursive call, step of the first move is decresed by
;; one and the move is removed from lom once all steps are performed. So Haulting
;; measure will decrease at each recursive call and function terminates once it reaches
;; to zero.
(define (execute-plan curr blocks lom)
  (cond
    [(my-member? curr blocks) false]
    [(empty? lom) curr]
    [else (execute-plan (next-pos-from-direction curr (move-dir (first lom)))
                        blocks
                        (moves-after-eval-one-move lom))]))


;; next-pos-from-direction : Position Direction -> Position
;; GIVEN: the current position and the direction to move
;; RETURNS: the next position after moving one step in given direction
;; EXAMPLES: (next-pos-from-direction (list 1 2) NE) = (list 2 3)
;; DESIGN STRATEGY: Use template for Direction on dir
(define (next-pos-from-direction curr dir)
  (cond
    [(string=? dir NE) (make-pos (add1 (pos-x curr)) (add1 (pos-y curr)))]
    [(string=? dir SE) (make-pos (add1 (pos-x curr)) (sub1 (pos-y curr)))]
    [(string=? dir SW) (make-pos (sub1 (pos-x curr)) (sub1 (pos-y curr)))]
    [(string=? dir NW) (make-pos (sub1 (pos-x curr)) (add1 (pos-y curr)))]))

;; moves-after-eval-one-move : Move Plan -> Plan
;; GIVEN: a Plan
;; RETURNS: the plan after decreasing one step in the first move if it is greater than
;; one else returns the rest of the moves of plan 
;; EXAMPLES:
#|
(moves-after-eval-one-move (list (list SE 2) (list NE 2)))
  = (list (list SE 1) (list NE 2))
|#
;; DESIGN STRATEGY: Cases on weather the steps of the first move are greater than
;;                  ONE-STEP or not
(define (moves-after-eval-one-move lom)
  (if (> (move-steps (first lom)) ONE-STEP)
      (cons (make-move (move-dir (first lom)) (sub1 (move-steps (first lom))))
                       (rest lom))
      (rest lom)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (check-false
   (path (list 2 5) (list 2 6) empty)
   "Robot should not reach to the destination point of different wing.")
  
  (check-false
   (path (list 2 5) (list 4 9) BLOCKS-1)
   "Robot should not reach to desination point due to no available path.")
  
  (check-equal?
   (path (list 2 5) (list 5 8) empty)
   (list (list NE 3))
   "Robot should reach to destination point.")

  (check-false
   (next-posn-from-posns empty (list 5 8))
   "No position is there to reach to the destination point.")

  (check-true
   (positions-on-same-wing? (list 2 8) (list 3 9))
   "Position are in the same wing.")

  (check-false
   (path (list 2 5) (list 3 6) (list (list 2 5)))
   "Robot on blocked position should not reach to the destination point.")

  (check-false
   (path (list 2 5) (list 3 6) (list (list 3 6)))
   "Robot should not reach to the blocked destination point.")
  
  (check-equal?
   (eval-plan (list 2 5) (rest BLOCKS-1) (path (list 2 5) (list 4 9) (rest BLOCKS-1)))
   (list 4 9)
   "Robot should reach to destination point by following the generated plan.")
  
  (check-equal?
   (eval-plan (list -3 6) BLOCKS-2 (path (list -3 6) (list 7 6) BLOCKS-2))
   (list 7 6)
   "Robot should reach to destination point by following the generated plan.")

  (check-false
   (eval-plan (list -3 6) (list (list -3 6)) empty)
   "Robot on blocked point can should reach to destination."))