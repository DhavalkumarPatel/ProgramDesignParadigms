#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "Controller.rkt")
(require "extras.rkt")

(provide MouseController%)

;; A MouseController is a
;; (new MouseController% [model Model] [width Nat] [height Nat] [half-width Nat]
;;             [half-height Nat] [display-width Nat] [display-height Nat] [handler-x Nat]
;;             [handler-y Nat] [x Nat] [y Nat] [handler-side Nat] [particle-x Nat]
;;             [particle-y Nat])
;; A MouseController% represents a mouse controller class which extends Controller class
;; and implemets its hook methods.
(define MouseController%
  (class* Controller% (Controller<%> SubControllerHooks<%> MouseController<%>)
    
    ;; model where controller is registered
    (inherit-field model)
    
    ;; Height and width of the controller
    (inherit-field width)
    (inherit-field height)
    (init-field display-width)
    (init-field display-height)
    (inherit-field half-width)
    (inherit-field half-height)
    
    ;; x and y coordinates of the center of the handler using which controller will be
    ;; draged
    (inherit-field handler-x)
    (inherit-field handler-y)
    
    ;; side of the handler
    (inherit-field handler-side)
    
    ;; x and y coordinates of the center of the rectangle which represents the controller
    ;; on canvas
    (inherit-field x y)
    
    ;; the position of the center of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)
    
    ;; fields for dragging
    ;; If there has ever been a button-down in this object, then these contain the
    ;; position of last button-down relative to center of viewer. Else any value.
    (inherit-field selected?)
    (inherit-field handler-selected?)
    (inherit-field saved-mx)
    (inherit-field saved-my)
    
    ;; inherited constants from the superclass
    (inherit-field RED-COLOR)
    (inherit-field BLACK-COLOR)
    (inherit-field BLUE-COLOR)
    (inherit-field OUTLINE-TYPE)
    (inherit-field SOLID-TYPE)
    (field [WHITE_COLOR "white"])
    
    ;; Radius of the circles which depicts the particle on canvas
    (field [DOT-CIRCLE-RADIUS 2])
    (field [OUTER-CIRCLE-RADIUS 10])
    (field [half-display-width (/ display-width 2)])
    (field [half-display-height (/ display-height 2)])
    
    
    (super-new)
    
    ;; Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this particle to the state it should have
    ;; following the button down event at the given location.
    ;; STRATEGY: wheather the mouse position is inside the rectangle object or not.
    (define/public (select-controller mx my)
      (if (super in-this? x y mx my half-display-width half-display-height)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx particle-x))
            (set! saved-my (- my particle-y))
            (send model execute-command #t))
          VOID))
    
    ; after-button-up : Nat Nat -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes this unselected
    (define/override (after-button-up mx my)
      (super after-button-up mx my)
      (send model execute-command #f))
    
    ;; to be supplied by the subclasses.
    (abstract drag-particle)
    
    ;; inherited from the parent class.
    (inherit/super in-this?)
    
    ;; -> Void
    (define/override (after-key-event kev) VOID)
    
    ;; Number^3 -> Number
    ;; GIVEN: three numbers lo, val and hi
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define/public (within-limits lo val hi)
      (max lo (min val hi)))
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a Scene like the given one, but with this controller painted on it.
    ;; STRATEGY: Combine simpler functions
    (define/override (add-to-scene scene)
      (place-image (rectangle width height OUTLINE-TYPE BLUE-COLOR) x y
                   (place-image (rectangle display-width
                                           display-height
                                           OUTLINE-TYPE BLACK-COLOR)
                                x y  scene)))
    ))