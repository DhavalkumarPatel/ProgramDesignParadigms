#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "extras.rkt")

(provide Controller%)

;; A Controller is a
;; (new Controller% [model Model] [width Nat] [height Nat] [half-width Nat]
;;                  [half-height Nat] [handler-x Nat] [handler-y Nat]
;;                  [x Nat] [y Nat])
;; A Controller represents a controller class which will be extended by all the
;; controllers who lives in the world.
(define Controller%
  (class* object% (Controller<%>)

    ;; model where controller is registered
    (init-field model)
    
    ;; Height and width of the controller
    (init-field width height)
    (init-field half-width)
    (init-field half-height)

    ;; x and y coordinates of the center of the handler using which controller will be
    ;; draged
    (init-field handler-x)
    (init-field handler-y)

    ;; x and y coordinates of the center of the rectangle which represents the controller
    ;; on canvas
    (init-field [x CANVAS-CENTER-X] [y CANVAS-CENTER-Y])

    ;; private fields of the class
    (field [HANDLER-HALF-WIDTH  5])
    (field [HANDLER-HALF-HEIGHT 5])
    (field [handler-side  10])
    
    ;; the position and velocities of the particle
    (field [particle-x 0])
    (field [particle-y 0])
    (field [particle-vx 0])
    (field [particle-vy 0])
    
    ;; fields for dragging
    ;; It there has ever been a button-down in this object, then these contain the
    ;; position of last button-down relative to center of viewer. Else any value.
    (field [selected? false])
    (field [handler-selected? false])
    (field [saved-mx 0])
    (field [saved-my 0])

    ;; display colors
    (field [RED-COLOR "red"])
    (field [BLACK-COLOR "black"])
    (field [BLUE-COLOR "blue"])

    ;; display-image-modes
    (field [OUTLINE-TYPE "outline"])
    (field [SOLID-TYPE "solid"])

    (super-new)

    ;; register this controller with the model
    (send model register this)
    
    ;; Signal -> Void
    ;; decodes signal and updates local data
    ;; STRATEGY: Cases on Signal sig
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (set! particle-x (report-position-pos-x sig))
         (set! particle-y (report-position-pos-y sig))]
        [(report-velocity? sig)
         (set! particle-vx (report-velocity-vx sig))
         (set! particle-vy (report-velocity-vy sig))]))

    
    ;; after-button-down : Nat Nat -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this controller to the state it should have following the button  
    ;;         down mouse event at the given location. If the mouse location is inside
    ;;         the handler then it selects the controller.
    ;; STRATEGY: Cases on whether the mouse is in the handler
    (define/public (after-button-down mx my)
      (if (in-handler? mx my) 
          (begin
            (set! handler-selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          (send this select-controller mx my)))

    ;; after-button-up : Nat Nat -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this controller to the state it should have following the button up
    ;;         mouse event at the given location.
    ;; EXAMPLE: controller's handler gets unselected after button up event.
    ;; STRATEGY: Combine Simpler functions
    (define/public (after-button-up mx my)
      (set! selected? false)
      (set! handler-selected? false))

    ;; in-handler? : Nat Nat -> Boolean
    ;; GIVEN: an x and y coordinates
    ;; RETURNS: true iff the given position is inside the handler
    (define (in-handler? mx my)
      (and
       (<= (- handler-x HANDLER-HALF-WIDTH) mx (+ handler-x HANDLER-HALF-WIDTH))
       (<= (- handler-y HANDLER-HALF-HEIGHT) my (+ handler-y HANDLER-HALF-HEIGHT))))

    ;; after-drag : Nat Nat -> Void
    ;; GIVEN: an x and a y coordinate
    ;; EFFECT: updates this controller to the state it should have following the drag
    ;;         mouse event at the given location.
    ;; EXAMPLE: If controller is selected at the handler then it drags the whole
    ;; controller else if the particle is selected then drags the particle.
    ;; STRATEGY: Cases on whether the controller is selected at handler or not
    (define/public (after-drag mx my)
      (if handler-selected?
          (begin
            (set! handler-x (+ handler-x (- mx saved-mx x)))
            (set! handler-y (+ handler-y (- my saved-my y)))
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          (send this drag-particle mx my)))

    ;; current-color : Boolean -> Color
    ;; RETURNS: returns red color if the given boolean is true else black
    (define/public (current-color bool)
      (if bool RED-COLOR BLACK-COLOR))

    ;; -> Void
    (define/public (after-tick) VOID)

    ;; to be supplied by the subclasses.
    (abstract add-to-scene)
    (abstract after-key-event)

    ;; Nat Nat Nat Nat Nat Nat -> Void
    ;; GIVEN: x and y coordinate of any point in canvas, x and y coordinate of center of
    ;;        the rectangle and width and height of that rectangle
    ;; RETURNS: true iff the given point is in side the given rectangle
    (define/public (in-this? a b other-x other-y w h)
      (and
       (<= (- a w) other-x (+ a w))
       (<= (- b h) other-y (+ b h)))) 
    ))