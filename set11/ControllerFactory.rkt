#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "XYController.rkt")
(require "ModelForTest.rkt")
(require "ParticleWorld.rkt")

(provide ControllerFactory%)

;; A ControllerFactory is a
;; (new ControllerFactory% [w World] [m Model])
;; A ControllerFactory represents a factory to create new controllers to interact with
;; model.
(define ControllerFactory%
  (class* object% (SWidget<%>)
    
    ; the world in which the controllers will live
    (init-field w)
    
    ; the model to which the controllers will be connected
    (init-field m)
    
    (super-new)
    
    ;; KeyEvent -> Void
    ;; EFFECT: adds a new controller according to the key event in world.
    ;; STRATEGY: Cases on KeyEvent kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "v") (add-viewer VelocityController%)]
        [(key=? kev "p") (add-viewer PositionController%)]
        [(key=? kev "x") (add-viewer XController%)]
        [(key=? kev "y") (add-viewer YController%)]
        [(key=? kev "z") (add-viewer XYController%)]
        [else this]))
    
    ;; Class -> Void
    ;; EFFECT: adds the given controller in world.
    (define/public (add-viewer viewer-class)
      (send w add-widget (new viewer-class [model m])))
    
    ;; Scene -> Scene
    ;; RETURNS: the given scene
    (define/public (add-to-scene s) s)
    
    ;; -> Void
    ;; EFFECT: No effects
    (define/public (after-tick) VOID)
    
    ;; Nat Nat -> Void
    ;; GIVEN: a mouse position
    ;; EFFECT: No effects
    (define/public (after-button-down mx my) VOID)
    
    ;; Nat Nat -> Void
    ;; GIVEN: a mouse position
    ;; EFFECT: No effects
    (define/public (after-drag mx my) VOID)
    
    ;; Nat Nat -> Void
    ;; GIVEN: a mouse position
    ;; EFFECT: No effects
    (define/public (after-button-up mx my) VOID)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define m (new Model%))
     (define w (make-world m CANVAS-WIDTH CANVAS-HEIGHT))
     (define cf (new ControllerFactory% [w w] [m m])))
    
    (send cf after-key-event "v")
    (send cf after-key-event "p")
    (send cf after-key-event "x")
    (send cf after-key-event "y")
    (send cf after-key-event "z")
    (send cf after-key-event "a")
    
    (check-equal?
     (length (send w for-test:widgt-list))
     5
     "After Key Event.")
    
    ;; Methods with no effects
    (send cf add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    (send cf after-tick)
    (send cf after-button-down 50 50)
    (send cf after-drag 50 50)
    (send cf after-button-up 50 50)))