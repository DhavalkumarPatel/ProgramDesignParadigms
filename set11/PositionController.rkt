#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "PosVelController.rkt")
(require "ModelForTest.rkt")
(require "extras.rkt")

(provide PositionController%)

;; A PositionController is a
;; (new PositionController% [model Model] [particle-x Nat] [particle-y Nat])
;; A PositionController% represents a Position controller class which extends
;; PosVelController class and implemets its hook methods.
(define PositionController%
  (class* PosVelController% (Controller<%> PosVelControllerHooks<%>)

    ;; model where controller is registered
    (inherit-field model)

    ;; the position of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)
    (field [POSITION-STRING "position"])

    (super-new)

    ;; Nat Nat -> Command
    ;; RETURNS: the position command for model to execute based on given x,y coordinates
    ;; of particle.
    (define/public (key-event-action px py)
      (make-set-position (+ particle-x px) (+ particle-y py)))

    ;; -> String
    ;; RETURNS: the name of controller to display
    (define/public (get-name) POSITION-STRING)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define pc (new PositionController% [model (new Model%)])))
    
    (check-equal?
     (send pc key-event-action CANVAS-CENTER-X CANVAS-CENTER-Y)
     (make-set-position 375 300)
     "After Key Event Action.")
    
    (check-equal?
     (send pc get-name)
     "position"
     "Get Name.")))