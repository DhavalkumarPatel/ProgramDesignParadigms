#lang racket

(require "WidgetWorks.rkt")

(provide PlaygroundState<%>
         Target<%>
         Toy<%>
         Block<%>
         CANVAS-WIDTH
         CANVAS-HEIGHT
         HALF-CANVAS-WIDTH
         HALF-CANVAS-HEIGHT
         INIT-BLOCK-X
         INIT-BLOCK-Y)


;; CONSTANTS:

;; Canvas Properties:
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define HALF-CANVAS-WIDTH (/ CANVAS-WIDTH 2))
(define HALF-CANVAS-HEIGHT (/ CANVAS-HEIGHT 2))
(define INIT-BLOCK-X (/ CANVAS-WIDTH 2))
(define INIT-BLOCK-Y (/ CANVAS-HEIGHT 2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;INTERFACES

;; Every object that lives in the playground must implement the PlaygroundState<%>
(define PlaygroundState<%>
  (interface (SWidget<%>);; this means: include all the methods in SWidget<%>   
    
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


;; Every target that lives in the playground must implement this interface
(define Target<%>
  (interface (SWidget<%>);; this means: include all the methods in SWidget<%>. 
    
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


;; Every toy present in the playground must implement this interface
(define Toy<%> 
  (interface (SWidget<%>)  ;; this means: include all the methods in SWidget<%>
    
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


;; Every block present in block factory must implement this interface
(define Block<%>
  (interface (SWidget<%>)
    
    ;; get-team : -> ListOfBlock<%>
    ;; GIVEN: no arguments
    ;; RETURNS: the teammates of this block
    get-team
    
    ;; add-teammate: Block<%> -> Void
    ;; GIVEN: a Block<%>
    ;; EFFECT: adds the given block to this block's team and vise versa
    add-teammate
    
    ;; block-x : -> Integer
    ;; block-y : -> Integer
    ;; RETURNS: the x or y coordinates of this block
    block-x
    block-y

    ;; update-other-blocks: Block<%> -> Void
    ;; GIVEN: a Block<%>
    ;; EFFECT: adds the given block to other blocks field
    update-other-blocks

    ;; move-by-distance: Int Int -> Void
    ;; GIVEN: distances along x and y axis
    ;; EFFECT: Updates the x and y co-ordinates so that the block moves by given distance 
    move-by-distance

    ;; set-team-mates-without-self: ListOfBlock<%> -> Void
    ;; GIVEN: a List of Block<%>
    ;; EFFECT: Updates the team mates of current block with given list,
    ;;         filtering itself out
    set-team-mates-without-self

    ))