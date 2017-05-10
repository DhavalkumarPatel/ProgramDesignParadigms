#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "Controller.rkt")
(require "extras.rkt")

(provide PosVelController%)

;; A PosVelController is a
;; (new PosVelController% [model Model] [width Integer] [height Integer]
;;             [half-width Integer] [half-height Integer] [display-width Integer]
;;             [display-height Integer] [handler-x Integer] [handler-y Integer])
;; A PosVelController% represents a Position and Velocity controller class which
;; extends Controller class and implemets its hook methods.
(define PosVelController%
  (class* Controller% (Controller<%> SubControllerHooks<%>)

    ;; model where controller is registered
    (inherit-field model)
    
    ;; x and y coordinates of the center of the rectangle which represents the controller
    ;; on canvas
    (inherit-field x y)   

    ;; Height and width of the controller
    (inherit-field width height)
    (inherit-field half-width)
    (inherit-field half-height)

    ;; x and y coordinates of the center of the handler using which controller will be
    ;; draged
    (inherit-field handler-x)
    (inherit-field handler-y)

    ;; side of the handler
    (inherit-field handler-side)
    
    ;; the position of the center of the particle
    (inherit-field particle-x)
    (inherit-field particle-y)

    ;; velocities of the particle in x and y directions in pixel/tic
    (inherit-field particle-vx)
    (inherit-field particle-vy)
    
    ;; fields for dragging
    ;; If there has ever been a button-down in this object, then these contain the
    ;; position of last button-down relative to center of viewer. Else any value.
    (inherit-field selected?)
    (inherit-field handler-selected?)

    ;; inherited constants from the superclass
    (inherit-field BLACK-COLOR)
    (inherit-field OUTLINE-TYPE)

    ;; Constants to display the viewer for the controller
    (field [TEXT-SIZE 12])
    (field [DISPLAY-WIDTH 150])
    (field [DISPLAY-HEIGHT 50])

    ;; Arrow keys 
    (field [UP-KEY "up"])
    (field [DOWN-KEY "down"])
    (field [LEFT-KEY "left"])
    (field [RIGHT-KEY "right"])
    ;; factor used to change postion or velocity after key event
    (field [CHANGE-FACTOR 5])
    
    (super-new [width DISPLAY-WIDTH] [height DISPLAY-HEIGHT]
               [half-width (/ DISPLAY-WIDTH  2)] [half-height (/ DISPLAY-HEIGHT 2)]
               [handler-x (+ HANDLER-HALF-WIDTH
                             (- CANVAS-CENTER-X (/ DISPLAY-WIDTH  2)))]
               [handler-y (+ HANDLER-HALF-HEIGHT
                             (- CANVAS-CENTER-Y (/ DISPLAY-HEIGHT 2)))])
    
    ;; Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this particle to the state it should have
    ;; following the button down event at the given location.
    ;; STRATEGY: wheather the mouse position is inside the rectangle object or not.
    (define/public (select-controller mx my)
      (if (super in-this? x y mx my half-width half-height)
          (set! selected? true)
          VOID)) 

    ;; inherited from the parent class.
    (inherit/super in-this?)
    
    ;; add-to-scene : Scene -> Scene
    ;; RETURNS: a scene like the given one, but with the viewer for this controller
    ;; painted on it.
    ;; STRATEGY: Place the image centered at x y
    (define/override (add-to-scene scene)
      (place-image
       (rectangle handler-side handler-side OUTLINE-TYPE
                  (super current-color handler-selected?))
       handler-x
       handler-y
       (place-image (viewer-image) x y scene)))
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this controller to the state it should have
    ;; following the given key event. Based on the KeyEvent it updates
    ;; the position/velocity of the particle by CHANGE-FACTOR.
    ;; STRATEGY: Cases on KeyEvent kev
    (define/override (after-key-event kev)
      (if selected?
          (cond
            [(key=? RIGHT-KEY kev)
             (send model execute-command
                   (send this key-event-action CHANGE-FACTOR 0))]
            [(key=? LEFT-KEY kev)
             (send model execute-command
                   (send this key-event-action (- CHANGE-FACTOR) 0))]
            [(key=? UP-KEY kev)
             (send model execute-command
                   (send this key-event-action 0 (- CHANGE-FACTOR)))]
            [(key=? DOWN-KEY kev)
             (send model execute-command
                   (send this key-event-action 0 CHANGE-FACTOR))])
          2345))
    
    ;; viewer-image : -> Image
    ;; GIVEN: no arguments
    ;; RETURNS: assemble the image of the viewer.
    (define (viewer-image)
      (let ((the-data-image (data-image)))
        (overlay the-data-image
                 (rectangle width height OUTLINE-TYPE (super current-color selected?)))))

    ;; Methods defined to call the super class methods
    (define/override (current-color a) VOID)
    (define/public (drag-particle mx my) VOID)
    
    ;; data-image : -> Image
    ;; GIVEN: no arguments
    ;; RETURNS: an image with the text data which represents the velocity or the position
    ;;          of the particle.
    ;; STRATEGY: Combine simpler functions
    (define (data-image)
      (above
       (text (string-append "Arrow keys change " (send this get-name)) 10 BLACK-COLOR)
       (text (string-append
              "X = "
              (number->string particle-x)
              " Y = "
              (number->string particle-y))
             TEXT-SIZE BLACK-COLOR)
       (text (string-append  
              " VX = "
              (number->string particle-vx)
              " VY = "
              (number->string particle-vy))
             TEXT-SIZE BLACK-COLOR)))    
    ))

