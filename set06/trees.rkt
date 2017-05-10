;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; trees.rkt: creates and manipulates trees on a canvas.

;; User can create a new root node with "t" key and new son for selected node
;; by "n" key. Hitting "d" while a node is selected deletes the node and its
;; all subtrees. Hitting "l" at any time deletes every node whose center is
;; in the left half of the canvas and their subtrees.

;; User can drag the entire tree rooted at that node by selecting it.
;; button-down to select, drag to move, button-up to release.

;; start with (run 0)

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

;; check the location of file for automated testing
;; (check-location "06" "trees.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS

;; dimensions of the canvas in pixels
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))

;; image of the empty canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; properties of the circle that represents node of the tree on canvas
(define CIRCLE-RADIUS 10)
(define OUTLINE "outline")
(define SOLID "solid")
(define COLOR-GREEN "green")
(define COLOR-BLUE "blue")

;; x and y distances for calculating center of new node
(define X-DISTANCE (* 3 CIRCLE-RADIUS))
(define Y-DISTANCE (* 3 CIRCLE-RADIUS))

;; any value for calling function who ignores the argument
(define ANY-VALUE 0)

;; initial and default values
(define INITIAL-X HALF-CANVAS-WIDTH)
(define INITIAL-Y CIRCLE-RADIUS)
(define INITIAL-SEL false)
(define DEFAULT-MX 0)
(define DEFAULT-MY 0)

;; lowest value of x for calculating maximum x-coordinate
(define LOWEST-X 0)

;; images for testing
(define CIRCLE-SEL-IMAGE (circle CIRCLE-RADIUS SOLID COLOR-GREEN))
(define CIRCLE-IMAGE (circle CIRCLE-RADIUS OUTLINE COLOR-GREEN))

(define RIGHT-NODE-IMAGE
  (scene+line
   (place-image
    CIRCLE-IMAGE (+ INITIAL-X X-DISTANCE) (+ INITIAL-Y Y-DISTANCE) EMPTY-CANVAS)
   INITIAL-X INITIAL-Y (+ INITIAL-X X-DISTANCE) (+ INITIAL-Y Y-DISTANCE) COLOR-BLUE))

(define RIGHT-AND-LEFT-NODE-IMAGE
  (scene+line
   (place-image
    CIRCLE-IMAGE (- INITIAL-X X-DISTANCE) (+ INITIAL-Y Y-DISTANCE) RIGHT-NODE-IMAGE)
   INITIAL-X INITIAL-Y (- INITIAL-X X-DISTANCE) (+ INITIAL-Y Y-DISTANCE) COLOR-BLUE))

(define TREE-IMAGE 
  (place-image CIRCLE-IMAGE INITIAL-X INITIAL-Y RIGHT-AND-LEFT-NODE-IMAGE))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

(define-struct node (x y selected? mx my))
;; A Node is a (make-node Integer Integer Boolean Integer Integer)
;; INTERPRETATION:
;; x and y are the co-ordinates of the center of the circle that represents node
;; on the canvas.
;; selected? represents whether or not the node is selected by mouse.
;; mx and my are the x and y co-ordinates of the mouse if node is selected else
;; they are DEFAULT-MX & DEFAULT-MY.

;; TEMPLATE:
;; node-fn : Node -> ??
#|
(define (node-fn n)
  (...
   (node-x n)
   (node-y n)
   (node-selected? n)
   (node-mx n)
   (node-my n)))
|#

;; examples of Node, for testing
(define INITIAL-NODE 
  (make-node INITIAL-X INITIAL-Y INITIAL-SEL DEFAULT-MX DEFAULT-MY))

(define LEFT-CHILD-NODE
  (make-node
   (- INITIAL-X X-DISTANCE) (+ INITIAL-Y Y-DISTANCE) INITIAL-SEL DEFAULT-MX DEFAULT-MY))
  
(define RIGHT-CHILD-NODE
  (make-node
   (+ INITIAL-X X-DISTANCE) (+ INITIAL-Y Y-DISTANCE) INITIAL-SEL DEFAULT-MX DEFAULT-MY))
  
(define INITIAL-NODE-SEL
  (make-node INITIAL-X INITIAL-Y true INITIAL-X INITIAL-Y))

(define INITIAL-NODE-SEL-MOVED
  (make-node (+ INITIAL-X 5) (+ INITIAL-Y 5) true (+ INITIAL-X 5) (+ INITIAL-Y 5)))
  
(define NEW-CHILD-NODE
  (make-node (+ INITIAL-X X-DISTANCE X-DISTANCE) (+ INITIAL-Y Y-DISTANCE)
             INITIAL-SEL DEFAULT-MX DEFAULT-MY))


(define-struct tree (root sons))
;; A Tree is a (make-tree Node ListOfTree)
;; INTERPRETATION:
;; root is a node at which the tree is rooted.
;; sons represents the list of immediate subtrees rooted at the root node.

;; A ListOfTree is one of
;; -- empty
;; -- (cons Tree ListOfTree)

;; TEMPLATE:
;; tree-fn : Tree -> ??
#|
(define (tree-fn t)
  (...
   (node-fn (tree-root t))
   (lot-fn (tree-sons t))))
|#

;; lot-fn : ListOfTree -> ??
#|
(define (lot-fn lot)
  (cond
    [(empty? lot) ...]
    [else (... (tree-fn (first lot))
               (lot-fn (rest lot)))]))
|#

;; examples of Tree and ListOfTree, for testing
(define LEFT-SUB-TREE (make-tree LEFT-CHILD-NODE empty))
(define RIGHT-SUB-TREE (make-tree RIGHT-CHILD-NODE empty))
(define NEW-CHILD-TREE (make-tree NEW-CHILD-NODE empty))
(define SONS-OF-TREE (list LEFT-SUB-TREE RIGHT-SUB-TREE))
(define TREE (make-tree INITIAL-NODE SONS-OF-TREE))
(define TREE-SEL (make-tree INITIAL-NODE-SEL SONS-OF-TREE))


(define-struct world (trees))
;; A World is a (make-world ListOfTree)
;; INTERPRETATION:
;; trees are the list of trees in world state.

;; TEMPLATE:
;; world-fn : World -> ??
#|
(define (world-fn w)
  (...
   (world-trees w)))
|#

;; examples of World, for testing
(define WORLD (make-world (list TREE)))
(define WORLD-SEL (make-world (list TREE-SEL)))


;; examples of KeyEvents, for testing
(define T-KEY-EVENT "t")
(define N-KEY-EVENT "n")
(define D-KEY-EVENT "d")
(define L-KEY-EVENT "l")
(define Q-KEY-EVENT "q")


;; examples of MouseEvents, for testing
(define BUTTON-DOWN-EVENT "button-down")
(define DRAG-EVENT "drag")
(define BUTTON-UP-EVENT "button-up")
(define MOVE-EVENT "move")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; EXAMPLES: (initial-world ANY-VALUE) = (make-world empty)

;; DESIGN STRATEGY: Combine simpler function
(define (initial-world n)
  (make-world empty))

;; TESTS:
(begin-for-test
  (check-equal?
   (initial-world ANY-VALUE)
   (make-world empty)
   "Initial world should be created."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
;; EXAMPLES: (run ANY-VALUE) will run the simulation starting with the
;; initial state.

;; DESIGN STRATEGY: Combine simpler functions
(define (run n)
  (big-bang (initial-world ANY-VALUE)
            (on-draw world-to-scene)
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-to-scene : World -> Scene
;; GIVEN: the world w
;; RETURNS: the scene that portrays the given world.
;; EXAMPLES:
;; (world-to-scene WORLD) = TREE-IMAGE

;; DESIGN STRATEGY: Use HOF foldr on (world-trees w)
(define (world-to-scene w)
  (foldr tree-to-scene EMPTY-CANVAS (world-trees w)))


;; tree-to-scene : Tree Scene -> Scene
;; GIVEN: a tree and a scene
;; RETURNS: the scene like given one, but with the given tree painted on it.
;; The tree node is rendered as a circle of CIRCLE-RADIUS on canvas.
;; EXAMPLES:
;; (tree-to-scene TREE) = TREE-IMAGE

;; DESIGN STRATEGY: Use template for Tree on tree
(define (tree-to-scene tree scene)
  (place-image (root-to-scene (tree-root tree)) 
               (node-x (tree-root tree))
               (node-y (tree-root tree))
               (trees-to-scene (tree-sons tree)
                               (node-x (tree-root tree))
                               (node-y (tree-root tree))
                               scene)))


;; trees-to-scene : ListOfTree Integer Integer Scene -> Scene
;; GIVEN: a list of subtrees lot, x and y co-ordinates of the center of parent tree
;; root and a scene
;; RETURNS: a scene like given one, but with the given subtrees painted on it.
;; The subtree node is rendered as a circle of CIRCLE-RADIUS on canvas with a blue
;; line from the center of a parent tree node to the center of each subtree node.
;; EXAMPLES:
#|
(trees-to-scene SONS-OF-TREE INITIAL-X INITIAL-Y EMPTY-CANVAS) = RIGHT-AND-LEFT-NODE-IMAGE
|#
;; DESIGN STRATEGY: Use HOF foldr on lot
(define (trees-to-scene lot root-x root-y scene)
  (foldr
   ;; Tree Scene -> Scene
   ;; GIVEN: a subtree and a scene
   ;; RETURNS: a scene like given one, but with the given subtree painted on it.
   (lambda (tree scene)
     (scene+line (tree-to-scene tree scene) 
                 root-x
                 root-y
                 (node-x (tree-root tree))
                 (node-y (tree-root tree))
                 COLOR-BLUE))
   scene
   lot))


;; root-to-scene : Node -> Scene
;; GIVEN: a node of some tree
;; RETURNS: a scene like given one, but with the given node painted on it.
;; The node is rendered as a circle of CIRCLE-RADIUS and COLOR-GREEN on canvas
;; and if node is selected then circle's mode is SOLID else it is OUTLINE. 
;; EXAMPLES:
;; (root-to-scene INITIAL-NODE) = CIRCLE-IMAGE
;; (root-to-scene INITIAL-NODE-SEL) = CIRCLE-SEL-IMAGE

;; DESIGN STRATEGY: Use template for Node on root
(define (root-to-scene root)
  (circle CIRCLE-RADIUS
          (if (node-selected? root) SOLID OUTLINE)
          COLOR-GREEN))


;;TESTS:
(begin-for-test
  (check-equal?
   (world-to-scene WORLD)
   TREE-IMAGE
   "World should be displayed as a scene.")
  
  (check-equal?
   (tree-to-scene TREE EMPTY-CANVAS) 
   TREE-IMAGE
   "Tree should be displayed as a image.")
  
  (check-equal?
   (trees-to-scene SONS-OF-TREE INITIAL-X INITIAL-Y EMPTY-CANVAS)
   RIGHT-AND-LEFT-NODE-IMAGE
   "Subtrees should be displayed as a image.")
  
  (check-equal?
   (root-to-scene INITIAL-NODE)
   CIRCLE-IMAGE
   "Node should be displayed as a image.")
  
  (check-equal?
   (root-to-scene INITIAL-NODE-SEL)
   CIRCLE-SEL-IMAGE
   "Selected Node should be displayed as a image."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world, the x- and y-coordinates of a mouse and a mouse event
;; RETURNS: the state of the world after following the given mouse event at
;; that location.
;; EXAMPLES:
#|
(world-after-mouse-event WORLD INITIAL-X INITIAL-Y BUTTON-DOWN-EVENT) = WORLD-SEL
(world-after-mouse-event WORLD-SEL INITIAL-X INITIAL-Y BUTTON-UP-EVENT) = WORLD
(world-after-mouse-event WORLD INITIAL-X INITIAL-Y MOVE-EVENT) = WORLD
(world-after-mouse-event (make-world (list (make-tree INITIAL-NODE-SEL empty)))
(+ INITIAL-X 5) (+ INITIAL-Y 5) DRAG-EVENT) 
    = (make-world (list (make-tree INITIAL-NODE-SEL-MOVED empty)))
|#

;; DESIGN STRATEGY: Cases on MouseEvent mev
(define (world-after-mouse-event w mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN-EVENT) (world-after-button-down w mx my)]
    [(mouse=? mev DRAG-EVENT) (world-after-drag w mx my)]
    [(mouse=? mev BUTTON-UP-EVENT) (world-after-button-up w)]
    [else w]))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-mouse-event WORLD INITIAL-X INITIAL-Y BUTTON-DOWN-EVENT)
   WORLD-SEL
   "On mouse button down event node should be selected.")
  
  (check-equal?
   (world-after-mouse-event WORLD-SEL INITIAL-X INITIAL-Y BUTTON-UP-EVENT)
   WORLD
   "On mouse button up event node should be unselected.")
  
  (check-equal?
   (world-after-mouse-event WORLD INITIAL-X INITIAL-Y MOVE-EVENT)
   WORLD
   "On any other mouse event world should be unchanged.")
  
  (check-equal?
   (world-after-mouse-event (make-world (list (make-tree INITIAL-NODE-SEL empty)))
                            (+ INITIAL-X 5) (+ INITIAL-Y 5) DRAG-EVENT)
   (make-world (list (make-tree INITIAL-NODE-SEL-MOVED empty)))
   "On mousedrag event selected tree should be draged.")

  (check-equal?
   (world-after-mouse-event WORLD (+ INITIAL-X 5) (+ INITIAL-Y 5) DRAG-EVENT)
   WORLD
   "On mousedrag event selected tree should be draged."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-button-down : World Integer Integer -> World
;; GIVEN: a world and the x- and y-coordinates of a mouse
;; RETURNS: the state of the world after following a button-down event at the
;; given location.
;; EXAMPLES:
;; (world-after-button-down WORLD INITIAL-X INITIAL-Y) = WORLD-SEL

;; DESIGN STRATEGY: Use template for World on w
(define (world-after-button-down w mx my)
  (make-world (trees-after-button-down (world-trees w) mx my)))


;; trees-after-button-down : ListOfTree Integer Integer -> ListOfTree
;; GIVEN: a list of trees in world and the x- and y-coordinates of a mouse
;; RETURNS: the list of trees after following a button-down event at the given
;; location.
;; EXAMPLES:
;; (trees-after-button-down (list TREE) INITIAL-X INITIAL-Y) = (list TREE-SEL)

;; DESIGN STRATEGY: Use HOF map on lot
(define (trees-after-button-down lot mx my)
  (map
   ;; Tree -> Tree
   ;; GIVEN: a tree
   ;; RETURNS: a tree after following a button-down event.
   (lambda (tree)
     (tree-after-button-down tree mx my))
   lot))


;; tree-after-button-down : Tree Integer Integer -> Tree
;; GIVEN: a tree and the x- and y-coordinates of a mouse
;; RETURNS: the tree after following a button-down event at the given location.
;; EXAMPLES:
;; (tree-after-button-down TREE INITIAL-X INITIAL-Y) = TREE-SEL

;; DESIGN STRATEGY: Use template for Tree on tree
(define (tree-after-button-down tree mx my)
  (make-tree
   (root-after-button-down (tree-root tree) mx my)
   (trees-after-button-down (tree-sons tree) mx my)))


;; root-after-button-down : Node Integer Integer -> Node
;; GIVEN: a node of the tree and the x- and y-coordinates of a mouse
;; RETURNS: the node after following a button-down event at the given location.
;; It updates the selected?, mx and my fields of given node if the mouse location
;; is inside the circle that represents the given node on canvas. 
;; EXAMPLES:
;; (root-after-button-down INITIAL-NODE INITIAL-X INITIAL-Y) = INITIAL-NODE-SEL 

;; DESIGN STRATEGY: Use template for Node on root
(define (root-after-button-down root mx my)
  (if (in-circle? (node-x root) (node-y root) mx my)
      (make-node (node-x root)
                 (node-y root)
                 true
                 mx
                 my)
      root))


;; in-circle? : Integer Integer Integer Integer -> Boolean
;; GIVEN: the x- and y-coordinates of the center of circle, and the x- and
;; y-coordinates of a mouse
;; RETURNS: true if the given mouse coordinate is inside the circle centered
;; at given center.
;; EXAMPLES:
;; (in-circle? INITIAL-X INITIAL-Y (+ INITIAL-X 5) INITIAL-Y = true 

;; DESIGN STRATEGY: combine simpler functions
(define (in-circle? x y mx my)
  (<= (distance-between-points x y mx my) CIRCLE-RADIUS))


;; distance-between-points : Integer Integer Integer Integer -> PosReal
;; GIVEN: the x- and y-coordinates of the center of circle, and the x- and
;; y-coordinates of a mouse
;; RETURNS: the distance between the center of circle and the given mouse coordinates
;; using formula (square root of ((x2 - x1)^2 + (y2 - y1)^2)).
;; EXAMPLES:
;; (distance-between-points INITIAL-X INITIAL-Y (+ INITIAL-X 5) INITIAL-Y = 5 

;; DESIGN STRATEGY: combine simpler functions
(define (distance-between-points x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1))
           (sqr (- y2 y1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-drag : World Integer Integer -> World
;; GIVEN: a world and the x- and y-coordinates of a mouse
;; RETURNS: the world following a drag event at the given location.
;; EXAMPLES:
#|
(world-after-drag (make-world (list (make-tree INITIAL-NODE-SEL empty)))
(+ INITIAL-X 5) (+ INITIAL-Y 5)) 
    = (make-world (list (make-tree INITIAL-NODE-SEL-MOVED empty)))
|#
;; DESIGN STRATEGY: Use template for World on w
(define (world-after-drag w mx my)
  (make-world (trees-after-drag (world-trees w) mx my)))


;; trees-after-drag : ListOfTree Integer Integer -> ListOfTree
;; GIVEN: a list of trees and the x- and y-coordinates of a mouse
;; RETURNS: the list of trees following a drag event at the given location.
;; EXAMPLES:
#|
(trees-after-drag (list (make-tree INITIAL-NODE-SEL empty))
(+ INITIAL-X 5) (+ INITIAL-Y 5)) 
    = (list (make-tree INITIAL-NODE-SEL-MOVED empty))
|#
;; DESIGN STRATEGY: Use HOF map on lot
(define (trees-after-drag lot mx my)
  (map
   ;; Tree -> Tree
   ;; GIVEN: a tree
   ;; RETURNS: a tree after following a drag event.
   (lambda (tree)
     (tree-after-drag tree mx my
                      (node-selected? (tree-root tree))
                      (node-mx (tree-root tree))
                      (node-my (tree-root tree))))
   lot))


;; tree-after-drag : Tree Integer Integer Boolean Integer Integer -> Tree
;; GIVEN: a tree, the x- and y-coordinates of a mouse, boolean value root-selected?
;;   and the mouse coordinates of parent node root-mx root-my. If any of the parent
;;   node of given tree is selected then root-selected? is true and root-mx, root-my
;;   are the mouse coordinates of selected location else they are values of its own
;;   root node.
;; RETURNS: the tree after following a drag event at the given location.

;; EXAMPLES:
#|
(tree-after-drag (make-tree INITIAL-NODE-SEL empty) (+ INITIAL-X 5) (+ INITIAL-Y 5)
   true INITIAL-X INITIAL-Y) 
     = (make-tree INITIAL-NODE-SEL-MOVED empty)
|#

;; DESIGN STRATEGY: Use template for Tree on tree
(define (tree-after-drag tree mx my root-selected? root-mx root-my)
  (make-tree
   (root-after-drag (tree-root tree) mx my root-selected? root-mx root-my)
   (sons-after-drag (tree-sons tree) mx my root-selected? root-mx root-my)))


;; root-after-drag : Node Integer Integer Boolean Integer Integer -> Node
;; GIVEN: a node, the x- and y-coordinates of a mouse, boolean value root-selected?
;;   and the mouse coordinates of parent node root-mx root-my. If any of the parent
;;   node of given node is selected then root-selected? is true and root-mx, root-my
;;   are the mouse coordinates of selected location else they are values of the given
;;   root node.
;; RETURNS: the node after following a drag event at the given location.

;; EXAMPLES:
#|
(root-after-drag INITIAL-NODE-SEL INITIAL-X INITIAL-Y true INITIAL-X INITIAL-Y) 
    = INITIAL-NODE-SEL 
|#
;; DESIGN STRATEGY: Use template for Node on root
(define (root-after-drag root mx my root-selected? root-mx root-my)
  (if root-selected?
      (make-node (+ (node-x root) (- mx root-mx))
                 (+ (node-y root) (- my root-my))
                 (node-selected? root) mx my)
      root))


;; sons-after-drag : ListOfTree Integer Integer Boolean
;;                   Integer Integer -> ListOfTree
;; GIVEN: a list of subtrees, the x- and y-coordinates of a mouse, boolean value
;;   root-selected? and the mouse coordinates of parent node root-mx root-my.
;;   If any of the parent node of given sons is selected then root-selected?
;;   is true and root-mx, root-my are the mouse coordinates of selected location
;;   else they are values of the root node of the given sons.
;; RETURNS: the list of subtrees after following a drag event at the given location.

;; EXAMPLES:
#|
(sons-after-drag (list (make-tree INITIAL-NODE-SEL empty)) INITIAL-X INITIAL-Y
   true INITIAL-X INITIAL-Y)
      = (list (make-tree INITIAL-NODE-SEL empty)) 
|#
;; DESIGN STRATEGY: Use HOF map on lot
(define (sons-after-drag lot mx my root-selected? root-mx root-my)
  (map
   ;; Tree -> Tree
   ;; GIVEN: a subtree
   ;; RETURNS: a subtree after following a drag event.
   (lambda (tree)
     (if root-selected?
         (tree-after-drag tree mx my root-selected? root-mx root-my)
         (tree-after-drag tree mx my
                          (node-selected? (tree-root tree))
                          (node-mx (tree-root tree))
                          (node-my (tree-root tree)))))
   lot))

;; TESTS
(begin-for-test
  (check set-equal?
   (sons-after-drag (list (make-tree INITIAL-NODE-SEL empty))
                    INITIAL-X INITIAL-Y true INITIAL-X INITIAL-Y)
   (list (make-tree INITIAL-NODE-SEL empty))
   "On mousedrag event selected tree should be draged."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-button-up : World -> World
;; GIVEN: a world w
;; RETURNS: the world following a button-up event.
;; EXAMPLES:
;; (world-after-button-up WORLD-SEL) = WORLD

;; DESIGN STRATEGY: Use template for World on w
(define (world-after-button-up w)
  (make-world (trees-after-button-up (world-trees w))))


;; trees-after-button-up : ListOfTree -> ListOfTree
;; GIVEN: a list of trees in world
;; RETURNS: the list of trees following a button-up event.
;; EXAMPLES:
;; (trees-after-button-up (list TREE-SEL)) = (list TREE) 

;; DESIGN STRATEGY: Use HOF map on lot
(define (trees-after-button-up lot)
  (map tree-after-button-up lot))


;; tree-after-button-up : Tree -> Tree
;; GIVEN: a tree
;; RETURNS: the tree following a button-up event.
;; EXAMPLES:
;; (trees-after-button-up TREE-SEL) = TREE 

;; DESIGN STRATEGY: Use template for Tree on tree
(define (tree-after-button-up tree)
  (make-tree (make-node (node-x (tree-root tree))
                        (node-y (tree-root tree))
                        INITIAL-SEL
                        DEFAULT-MX
                        DEFAULT-MY)
             (trees-after-button-up (tree-sons tree))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w and a key event kev
;; RETURNS: the state of the world as it should be following the given key event
;; EXAMPLES:
#|
(world-after-key-event (make-world empty) T-KEY-EVENT)
    = (make-world (cons (make-tree INITIAL-NODE empty) empty))
(world-after-key-event WORLD-SEL N-KEY-EVENT)
    = (make-world (list (make-tree INITIAL-NODE-SEL (cons NEW-CHILD-TREE SONS-OF-TREE))))
(world-after-key-event WORLD-SEL D-KEY-EVENT)
    = (make-world empty)
(world-after-key-event WORLD-SEL L-KEY-EVENT)
    = (make-world (list (make-tree INITIAL-NODE-SEL (list RIGHT-SUB-TREE))))
(world-after-key-event WORLD-SEL Q-KEY-EVENT)
    = WORLD-SEL
|#
;; DESIGN STRATEGY: Cases on KeyEvent kev
(define (world-after-key-event w kev)
  (cond
    [(key=? kev T-KEY-EVENT) (world-after-t-key-event w)]
    [(key=? kev N-KEY-EVENT) (world-after-n-key-event w)]
    [(key=? kev D-KEY-EVENT) (world-after-d-key-event w)]
    [(key=? kev L-KEY-EVENT) (world-after-l-key-event w)]
    [else w]))

;; TESTS:
(begin-for-test
  (check-equal?
   (world-after-key-event (make-world empty) T-KEY-EVENT)
   (make-world (cons (make-tree INITIAL-NODE empty) empty))
   "New tree should be created at default position")

  (check-equal?
   (world-after-key-event WORLD-SEL N-KEY-EVENT)
   (make-world (list (make-tree INITIAL-NODE-SEL (cons NEW-CHILD-TREE SONS-OF-TREE))))
   "New child node should be created for selected node.")

  (check-equal?
   (world-after-key-event WORLD-SEL D-KEY-EVENT)
   (make-world empty)
   "Selected trees with subtrees should be deleted.")

  (check-equal?
   (world-after-key-event WORLD-SEL L-KEY-EVENT)
   (make-world (list (make-tree INITIAL-NODE-SEL (list RIGHT-SUB-TREE))))
   "Trees in left part of canvas should be deleted.")

  (check-equal?
   (world-after-key-event WORLD-SEL Q-KEY-EVENT)
   WORLD-SEL
   "World should be unchanged for any other key event."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-t-key-event : World -> World
;; GIVEN: a world w
;; RETURNS: the world after adding new tree in the given world.
;; EXAMPLES:
#|
(world-after-t-key-event (make-world empty))
    = (make-world (cons (make-tree INITIAL-NODE empty) empty))
|#
;; DESIGN STRATEGY: Use template for World on w
(define (world-after-t-key-event w)
  (make-world
   (cons
    (make-tree (make-node INITIAL-X INITIAL-Y INITIAL-SEL DEFAULT-MX DEFAULT-MY)
               empty)
    (world-trees w))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-n-key-event : World -> World
;; GIVEN: a world w
;; RETURNS: a world after adding a new son to the selected nodes of the trees
;; of given world.
;; EXAMPLES:
#|
(world-after-n-key-event WORLD-SEL)
    = (make-world (list (make-tree INITIAL-NODE-SEL (cons NEW-CHILD-TREE SONS-OF-TREE))))
|#
;; DESIGN STRATEGY: Use template for World on w
(define (world-after-n-key-event w)
  (make-world (trees-after-adding-new-son (world-trees w))))


;; trees-after-adding-new-son : ListOfTree -> ListOfTree
;; GIVEN: a list of trees in world lot
;; RETURNS: a list of trees after adding a new son to the selected nodes of given trees.
;; EXAMPLES:
#|
(trees-after-adding-new-son (list TREE-SEL))
    = (list (make-tree INITIAL-NODE-SEL (cons NEW-CHILD-TREE SONS-OF-TREE)))
|#
;; DESIGN STRATEGY: Use HOF map on lot
(define (trees-after-adding-new-son lot)
  (map tree-after-adding-new-son lot))


;; tree-after-adding-new-son : Tree -> Tree
;; GIVEN: a tree
;; RETURNS: a tree after adding a new son if its root node is selected.
;; EXAMPLES:
#|
(tree-after-adding-new-son TREE-SEL)
    = (make-tree INITIAL-NODE-SEL (cons NEW-CHILD-TREE SONS-OF-TREE))
|#
;; DESIGN STRATEGY: Use template for Tree on tree
(define (tree-after-adding-new-son tree)
  (make-tree
   (tree-root tree)
   (if (node-selected? (tree-root tree))
       (sons-after-adding-new-son (tree-root tree)
                                  (trees-after-adding-new-son (tree-sons tree)))
       (trees-after-adding-new-son (tree-sons tree)))))


;; sons-after-adding-new-son : Node ListOfTree -> ListOfTree
;; GIVEN: a node of tree at root and list of subtrees lot
;; RETURNS: a list of subtrees after adding a new subtree in given list of trees whose
;; center's x-coordinate is 3 radii to the right of the rightmost son and y-coordinate
;; is 3 radii down from the center of the parent. The x-coordinate of the first son of
;; a node is same as the parent node.
;; EXAMPLES:
;; (sons-after-adding-new-son SONS-OF-TREE) = (cons NEW-CHILD-TREE SONS-OF-TREE)

;; DESIGN STRATEGY: Use template for Node on root
(define (sons-after-adding-new-son root lot)
  (cons
   (make-tree
    (make-node (node-x-of-new-son (node-x root) lot) 
               (+ (node-y root) Y-DISTANCE)
               INITIAL-SEL
               DEFAULT-MX
               DEFAULT-MY)
    empty)
   lot))


;; node-x-of-new-son : Integer ListOfTree -> Integer 
;; GIVEN: the x-coordinate of the center of parent node root-x and list of subtrees
;; of parent node lot
;; RETURNS: the x-coordinate of a new son which is 3 radii to the right of the
;; rightmost son. if the new son is the first son then it will return the given
;; x-coordinate only.
;; EXAMPLES:
#|
(node-x-of-new-son INITIAL-X empty) = INITIAL-X
(node-x-of-new-son INITIAL-X SONS-OF-TREE) = (+ INITIAL-X X-DISTANCE X-DISTANCE)
|#
;; DESIGN STRATEGY: Use HOF foldr on lot
(define (node-x-of-new-son root-x lot)
  (if (empty? lot)
      root-x
      (+ (foldr
          ;; Tree Integer -> Tree
          ;; GIVEN: a subtree and current maximum x-coordinate
          ;; RETURNS: the x-coordinate of the center of rightmost son.
          (lambda (tree max-x) (max (node-x (tree-root tree)) max-x))
          LOWEST-X
          lot)
         X-DISTANCE)))

;; TESTS:
(begin-for-test
  (check-equal?
   (node-x-of-new-son INITIAL-X empty)
   INITIAL-X
   "node-x-of-new-son should return root-x for first son.")

  (check-equal?
   (node-x-of-new-son INITIAL-X SONS-OF-TREE)
   (+ INITIAL-X X-DISTANCE X-DISTANCE)
   "node-x-of-new-son should return x for new son.")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-after-d-key-event : World -> World
;; GIVEN: a world w
;; RETURNS: a world after deleting a tree whose node at root is selected from
;; the trees of given world.
;; EXAMPLES:
;; (world-after-d-key-event WORLD-SEL) = (make-world empty)

;; DESIGN STRATEGY: Use HOF trees-after-deleting-tree on (world-trees w) 
(define (world-after-d-key-event w)
  (make-world (trees-after-deleting-tree (world-trees w)
                                         node-selected?)))


;; world-after-l-key-event : World -> World
;; GIVEN: a world w
;; RETURNS: a world after deleting a tree whose center of node at root is in
;; the left half of the canvas from the trees of given world.
;; EXAMPLES:
#|
(world-after-l-key-event WORLD-SEL)
    = (make-world (list (make-tree INITIAL-NODE-SEL (list RIGHT-SUB-TREE))))
|#
;; DESIGN STRATEGY: Use HOF trees-after-deleting-tree on (world-trees w) 
(define (world-after-l-key-event w)
  (make-world (trees-after-deleting-tree (world-trees w)
                                         node-in-left-half-of-canvas?)))


;; trees-after-deleting-tree : ListOfTree (Node -> Boolean) -> ListOfTree
;; GIVEN: a list of trees in the world and condition function for deleting the tree.
;; RETURNS: a list of tree after deleting a tree from the given list of trees whose
;; node at root satisfies the given condition.
;; EXAMPLES:
#|
(trees-after-deleting-tree (list TREE-SEL) node-selected?) = empty
(trees-after-deleting-tree (list TREE-SEL) node-in-left-half-of-canvas?)
    = (list (make-tree INITIAL-NODE-SEL (list RIGHT-SUB-TREE)))
|#
;; DESIGN STRATEGY: Use HOF foldr on lot 
(define (trees-after-deleting-tree lot cond-fn)
  (foldr
   ;; Tree ListOfTree -> ListOfTree
   ;; GIVEN: a tree and a list of tree
   ;; RETURNS: a list of trees after deleting a tree from lot.
   (lambda (tree trees)
     (if (cond-fn (tree-root tree))
         trees
         (cons (tree-after-deleting-subtree tree cond-fn) trees)))
   empty
   lot))


;; tree-after-deleting-subtree : Tree (Node -> Boolean) -> Tree
;; GIVEN: a tree in the world and condition function for deleting the tree.
;; RETURNS: a tree after deleting a subtree whose node at root satisfies the given
;; condition function.
;; EXAMPLES:
#|
(tree-after-deleting-subtree TREE-SEL node-selected?) = empty
(tree-after-deleting-subtree TREE-SEL node-in-left-half-of-canvas?)
    = (make-tree INITIAL-NODE-SEL (list RIGHT-SUB-TREE))
|#
;; DESIGN STRATEGY: Use HOF trees-after-deleting-tree on (tree-sons tree)
(define (tree-after-deleting-subtree tree cond-fn)
  (make-tree (tree-root tree)
             (trees-after-deleting-tree (tree-sons tree) cond-fn)))


;; node-in-left-half-of-canvas? : Node -> Boolean
;; GIVEN: a node
;; RETURNS: whether the center of the given node is in left halh of the canvas or not.
;; EXAMPLES:
#|
(node-in-left-half-of-canvas? INITIAL-NODE) =  false
(node-in-left-half-of-canvas? LEFT-CHILD-NODE) = true
(node-in-left-half-of-canvas? RIGHT-CHILD-NODE) = false
|#
;; DESIGN STRATEGY: Use template for Node on root
(define (node-in-left-half-of-canvas? root)
  (< (node-x root) HALF-CANVAS-WIDTH))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; EXAMPLES:
;; (world-to-trees WORLD) = (list TREE)

;; DESIGN STRATEGY: Use template for World on w
(define (world-to-trees w)
  (world-trees w))

;; TESTS:
(begin-for-test
  (check set-equal?
   (world-to-trees WORLD)
   (list TREE)
   "world-to-trees should return the list of trees."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLES:
;; (tree-to-root TREE) = INITIAL-NODE

;; DESIGN STRATEGY: Use template for Tree on tree
(define (tree-to-root tree)
  (tree-root tree))

;; TESTS:
(begin-for-test
  (check-equal?
   (tree-to-root TREE)
   INITIAL-NODE
   "tree-to-root should return the root node of tree."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the immediate subtrees of the given tree.
;; EXAMPLES:
;; (tree-to-sons TREE) = SONS-OF-TREE

;; DESIGN STRATEGY: Use template for Tree on tree
(define (tree-to-sons tree)
  (tree-sons tree))

;; TESTS:
(begin-for-test
  (check set-equal?
   (tree-to-sons TREE)
   SONS-OF-TREE
   "tree-to-sons should return the sons of tree."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; node-to-center : Node -> Posn
;; RETURNS: the center of the given node as it is to be displayed on the
;; scene.
;; EXAMPLES:
;; (node-to-center INITIAL-NODE) = (make-posn INITIAL-X INITIAL-Y)

;; DESIGN STRATEGY: Use template for Node on node
(define (node-to-center node)
  (make-posn (node-x node) (node-y node)))

;; TESTS:
(begin-for-test
  (check-equal?
   (node-to-center INITIAL-NODE)
   (make-posn INITIAL-X INITIAL-Y)
   "node-to-center should return center point of the node."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; node-to-selected? : Node -> Boolean
;; RETURNS: true iff the given node is selected.
;; EXAMPLES:
;; (node-to-selected? INITIAL-NODE-SEL) = true
;; (node-to-selected? INITIAL-NODE) = false

;; DESIGN STRATEGY: Use template for Node on node
(define (node-to-selected? node)
  (node-selected? node))

;; TESTS:
(begin-for-test
  (check-true
   (node-to-selected? INITIAL-NODE-SEL)
   "NODE-RIGHT-PART-OF-CANVAS-SEL is selected.")
  
  (check-false
   (node-to-selected? INITIAL-NODE)
   "NODE-RIGHT-PART-OF-CANVAS is not selected."))