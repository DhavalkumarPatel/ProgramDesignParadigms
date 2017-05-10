#lang racket
;; Design a program to implement a world with blocks
;; where dragging a block onto another one joins them therafter

;; REQUIRE STATEMENTS
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "sets.rkt")
(require "extras.rkt")
(require "interfaces.rkt")
(require "WidgetWorks.rkt")
(require "Blocks.rkt")
(require "BlockFactory.rkt")

;; Provide statemetns
(provide make-block
         Block<%>
         StatefulWorld<%>
         Widget<%>
         SWidget<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-block : NonNegInt NonNegInt ListOfBlock<%> -> Block<%>
;; GIVEN: an x and y position, and a list of blocks
;; WHERE: the list of blocks is the list of blocks already on the playground.
;; RETURNS: a new block, at the given position, with no teammates
;; NOTE: it is up to you as to whether you use the third argument or
;; not.  Some implementations may use the third argument; others may not.

(define (make-block x y lob)
  (new Block% [x x] [y y][other-blocks lob]))


;; run : PosNum  -> Void 
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates and runs a world with given frame rate
(define (run frame-rate)
  (local
    ((define w (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define bf (new BlockFactory% [world w])))
    (begin
      (send w add-stateful-widget bf)
      (send w run frame-rate))))


;; UNIT TEST

(begin-for-test
  (local
    ((define f0 (make-block 100 100 empty)))
    
    (send f0 after-tick)
    
    (check-equal?
     (send f0 block-x)
     100) ))
