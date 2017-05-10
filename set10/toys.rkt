#lang racket
;; Design a program to make an interactive display of toys of various shapes
;; and different behaviours

;; start with (run 0.1 5) where 0.1 is frame-rate and 5 is square-speed

;; Require statements:
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces.rkt")
(require "PlaygroundState.rkt")
(require "Target.rkt")
(require "Square.rkt")
(require "Throbber.rkt")
(require "Football.rkt")
(require "Clock.rkt")

;; Provide
(provide make-world
         make-playground
         run
         make-square-toy
         make-throbber
         make-clock
         make-football
         PlaygroundState<%>
         Target<%>
         Toy<%>
         StatefulWorld<%>
         Widget<%>
         SWidget<%>)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; run : PosNum PosInt  -> Void 
;; GIVEN: a frame rate (in seconds/tick) and a square-speed (in pixels/tick)
;; EFFECT: creates and runs a world in which square toys travel at the given speed.
(define (run frame-rate square-speed)
  (local
    ((define world (make-world CANVAS-WIDTH CANVAS-HEIGHT))
     (define playground (make-playground  square-speed)))
    (begin
      (send world add-stateful-widget playground)
      (send world run frame-rate))))
