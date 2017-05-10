#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "Interfaces.rkt")
(require "PosVelController.rkt")
(require "extras.rkt")
(require "ModelForTest.rkt")

(provide VelocityController%)

;; A VelocityController is a (new VelocityController% [model Model])
;; A VelocityController% represents a Velocity controller class which extends
;; PosVelController class and implemets its hook methods.
(define VelocityController%
  (class* PosVelController% (Controller<%> PosVelControllerHooks<%>)
    
    ;; model where controller is registered
    (inherit-field model)
    
    ;; private fields of the class
    (field [VELOCITY-STRING "velocity"])
    
    (super-new)
    
    ;; Nat Nat -> Command
    ;; RETURNS: the position command for model to execute based on given x,y coordinates
    ;; of particle.
    (define/public (key-event-action px py)
      (make-incr-velocity px py))
    
    ;; -> String
    ;; RETURNS: the name of controller to display
    (define/public (get-name) VELOCITY-STRING)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define vc (new VelocityController% [model (new Model%)])))
    
    (check-equal?
     (send vc key-event-action CANVAS-CENTER-X CANVAS-CENTER-Y)
     (make-incr-velocity CANVAS-CENTER-X CANVAS-CENTER-Y)
     "After Key Event Action.")
    
    (check-equal?
     (send vc get-name)
     "velocity"
     "Get Name.")))
