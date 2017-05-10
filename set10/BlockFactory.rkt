#lang racket
;; BlockFactory.rkt : Class definition for BlockFactory class.

;; Require statements:
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "sets.rkt")
(require "extras.rkt")
(require "interfaces.rkt")
(require "WidgetWorks.rkt")
(require "Blocks.rkt")

;; Provide
(provide BlockFactory%)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:


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


;; A BlockFactory is a (new BlockFactory% [world WorldState<%>])
;; A BlockFactory represents a factory which contains all blocks living in the world.
;; accepts "b" key events and adds them to the world gets the world as an init-field
(define BlockFactory%
  (class* object% (SWidget<%>)

    ;;the world to which the factory adds blocks 
    (init-field world)  

    ;; key event for creating blocks
    (field [B-KEY-EVENT "b"])

    ;; list of blocks present in the world
    (field [blocks-list empty])

    ;; stores last x position of mouse up/down event
    (field [last-mx INIT-BLOCK-X])

    ;; stores last y position of mouse up/down event
    (field [last-my INIT-BLOCK-Y])
    
    (super-new)

    
    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a KeyEvent
    ;; EFFECTS: creates a new block if b key is pressed and updates the blocks list
    ;; Examples: See tests below
    ;; STRATEGY: Divide into cases on key-event kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev B-KEY-EVENT)
         (local
           ((define nb (new-block)))
           (begin
             (send world
                   add-stateful-widget nb)
             (set! blocks-list (cons nb blocks-list))))]))

    
    ;; new-block: -> Block<%>
    ;; GIVEN: no arguments
    ;; RETURNS: a new Block<%> centered at last mouse coordinates
    ;; Examples: See tests below
    ;; STRATEGY: Combine simpler functions
    (define (new-block)
      (new Block%
                      [x last-mx]
                      [y last-my]
                      [other-blocks blocks-list]))

    
    ;; update-mouse-coordinates: Nat Nat -> Void
    ;; GIVEN: a mouse co-ordiantes of mouse event
    ;; EFFECT: updates fields last-mx , last-my with given mouse coordinates
    ;; Examples: See tests below
    ;; STRATEGY: Combine simpler functions
    (define (update-mouse-coordinates mx my)
      (set! last-mx mx)
      (set! last-my my))

    
    ;; after-tick : -> BlockFactory<%>
    ;; GIVEN: no arguments
    ;; RETURNS: A BlockFactory like this but in a state as it should be after a tick
    ;; Examples: See tests below
    ;; DETAILS: a BlockFactory does not change after tick
    (define/public (after-tick) this)


    ;; after-button-down : Int Int -> BlockFactory<%>
    ;; GIVEN: a mouse co-ordiantes of mouse down event
    ;; RETURNS: A BlockFactory like this but in a state as it should be after a
    ;;          mouse down event.
    ;; EXAMPLE: A block factory registers a mouse button down at 100,100. function will
    ;;          store this mouse co-ordinates in last-mx ,last-my and return same block
    ;;          factory
    ;; STRATEGY: Combine simpler functions
    (define/public (after-button-down mx my)
      (begin
        (update-mouse-coordinates mx my)
        this))


    ;; after-button-up : Int Int -> BlockFactory<%>
    ;; GIVEN: a mouse co-ordiantes of mouse up event
    ;; RETURNS: A BlockFactory like this but in a state as it should be after a
    ;;          mouse up event.
    ;; EXAMPLE: A block factory registers a mouse button up at 100,100. function will
    ;;          store this mouse co-ordinates in last-mx ,last-my and return same block
    ;;          factory
    ;; STRATEGY: Combine simpler functions
    (define/public (after-button-up mx my)
      (begin
        (update-mouse-coordinates mx my) this))

    
    ;; after-drag : Int Int -> BlockFactory<%>
    ;; GIVEN: a mouse co-ordiantes of mouse drag event
    ;; RETURNS: A BlockFactory like this but in a state as it should be after a drag event
    ;; Examples: See tests below
    ;; DETAILS: a BlockFactory ignores drag event
    (define/public (after-drag mx my) this)

    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: the same scene
    ;; Examples: See tests below
    ;; DETAILS: block-factory does not add anything to scene
    (define/public (add-to-scene s) s)


    ;; Methods for Testing
    ;; -> ListOfBlocks<%>
    ;; Returns: list of blocks present in the world
    (define/public (for-test:blocks-list) blocks-list)

    
    ;; -> ListOfBlocks<%>
    ;; Returns: last mouse x coordinate
    (define/public (for-test:last-mx) last-mx)

    
    ;; -> ListOfBlocks<%>
    ;; Returns: last mouse y coordinate
    (define/public (for-test:last-my) last-my)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS FOR TESTING:
(define NEW-BLOCK-EVENT "b")
(define SIDE 20)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define f0 (new BlockFactory% [world (make-world 500 600)])))
    
    (send f0 after-tick)
    
    (check-equal?
     (send f0 for-test:blocks-list)
     empty
     "After tick no change") 
    
    (send f0 after-drag 100 100)
    
    (check-equal?
     (send f0 for-test:blocks-list)
     empty
     "After drag event no change")

    (send f0 after-button-up 200 100)
    
    (check-equal?
     (send f0 for-test:last-mx)
     200
     "After up event updated last-mx")

    (send f0 after-button-down 200 100)
    
    (check-equal?
     (send f0 for-test:last-my)
     100
     "After down event updated last-my")

    (send f0 after-key-event NEW-BLOCK-EVENT)
    
    (check-equal?
     (length (send f0 for-test:blocks-list))
     1
     "Newblock added after b key")
    
    (check-equal?
     (send f0 add-to-scene EMPTY-CANVAS)
     EMPTY-CANVAS
     "Image of block created on empty canvas ")

    (send f0 after-key-event NEW-BLOCK-EVENT)
    
    (check-equal?
     (length (send f0 for-test:blocks-list))
     2
     "Newblock added after b key")))