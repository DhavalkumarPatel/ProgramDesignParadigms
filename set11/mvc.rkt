#lang racket
;; Design a program to simulate a dimensionless particle bouncing in a 150x100 rectangle
;; using MVC architecture.

;; start with (run 0.1) where 0.1 is frame-rate

;; Require statements:
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "Model.rkt")
(require "ParticleWorld.rkt")
(require "ControllerFactory.rkt")

;; Provide
(provide run)


;; run : PosReal -> Void
;; GIVEN: a frame rate, in sec/tick
;; EFFECT: Creates and runs the MVC simulation with the given frame rate.
(define (run rate)
  (let* ((m (new Model%))
         (w (make-world m CANVAS-WIDTH CANVAS-HEIGHT)))
    (begin
      (send w add-widget
        (new ControllerFactory% [m m][w w]))
      (send w run rate))))