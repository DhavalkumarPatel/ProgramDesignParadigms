#lang racket

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "extras.rkt")
(require "MouseController.rkt")
(require "ModelForTest.rkt")

(provide XController%)

;; A XController is a
;; (new XController% [model Model] [x Nat] [y Nat] [half-width Nat] [half-height Nat]
;;                   [handler-x Nat] [handler-y Nat] [particle-x Nat] [particle-x Nat])
;; A XController% represents a X controller class which extends MouseController
;; class and implemets its hook methods.
(define XController%
  (class* MouseController% (Controller<%> MouseControllerHooks<%>)
    
    ;; model where controller is registered
    (inherit-field model)
    
    ;; x and y coordinates of the center of the rectangle which represents the controller
    ;; on canvas
    (inherit-field x y)
    
    ;; Half height and width of the controller
    (inherit-field half-width half-height)

    ;; Display height and width of the controller
    (inherit-field display-height)
    (inherit-field half-display-width)
    (inherit-field half-display-height)
    
    ;; x and y coordinates of the center of the handler using which controller will be
    ;; draged
    (inherit-field handler-x handler-y )
    
    ;; the position of the center of the particle
    (inherit-field particle-x particle-y)
    
    ;; inherited constants from the super class
    (inherit-field RED-COLOR)
    (inherit-field BLACK-COLOR)
    (inherit-field OUTLINE-TYPE)
    (inherit-field SOLID-TYPE)
    (inherit-field handler-side)
    (inherit-field DOT-CIRCLE-RADIUS)
    (inherit-field OUTER-CIRCLE-RADIUS)
    (inherit-field WHITE_COLOR)
   
    
    ;; fields for dragging
    ;; It there has ever been a button-down in this object, then these contain the
    ;; position of last button-down relative to center of viewer. Else any value.
    (inherit-field selected? handler-selected?)
    (inherit-field saved-mx saved-my)
    
    (super-new
     [width X-PARTICLE-LIMIT][height 50]
     [display-width (+ X-PARTICLE-LIMIT (* 2 DISPLAY-BUFFER))]
     [display-height 50]
     [half-width (/ X-PARTICLE-LIMIT 2)] [half-height (/ 50 2)]
     [handler-x (+ HANDLER-HALF-WIDTH
                   (- CANVAS-CENTER-X
                      (/ (+ X-PARTICLE-LIMIT (* 2 DISPLAY-BUFFER)) 2)))]
     [handler-y (+ HANDLER-HALF-HEIGHT (- CANVAS-CENTER-Y 25))])
    
    
    ;; Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this particle to the state it should have
    ;; following the drag event at the given location.
    ;; STRATEGY: Cases on weather particle is selected or not and drag is inside the
    ;; controller or not
    (define/override (drag-particle mx my)
      (if (and selected?
               (super in-this? x y mx my half-display-width half-display-height))
          (send model execute-command
                (make-set-position
                 (super within-limits 0 (- mx saved-mx) X-PARTICLE-LIMIT)
                 particle-y))
          VOID))
    
    ;; Methods defined to call the super class methods
    (define/override (within-limits a b c) VOID)   
    (define/override (in-this? a b c d e f) VOID)
    (define/override (current-color a) VOID)
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a Scene like the given one, but with this controller with the particle
    ;;          painted on it.
    ;; STRATEGY: Combine simpler functions
    (define/override (add-to-scene scene)
      (place-rectangles
       (place-image (circle DOT-CIRCLE-RADIUS SOLID-TYPE BLACK-COLOR)
                    (+ x (- particle-x half-width))
                    y 
                    (place-image (circle OUTER-CIRCLE-RADIUS SOLID-TYPE RED-COLOR)
                                 (+ x (- particle-x half-width))
                                 y
                                 (place-image (rectangle handler-side handler-side
                                                         OUTLINE-TYPE
                                                         (super current-color
                                                                handler-selected?))
                                              handler-x handler-y
                                              (super add-to-scene scene))))))

    ;; place-rectangles : Scene -> Scene
    ;; GIVEN: a Scene
    ;; RETURNS: a Scene like the given one, but two rectangles, one at right and
    ;;          one at left paindted on the given scene to hide the part
    ;;          of the circle which is going outside the controller.
    ;; STRATEGY: Combine simpler functions
    (define (place-rectangles scene)
      (place-image
       (rectangle (- (* 2 OUTER-CIRCLE-RADIUS) 2) (- display-height 2)
                  SOLID-TYPE WHITE_COLOR)
       (+ ( + x half-width) OUTER-CIRCLE-RADIUS) y
       (place-image (rectangle (- (* 2 OUTER-CIRCLE-RADIUS) 2) (- display-height 2 )
                               SOLID-TYPE WHITE_COLOR)
                    (- ( - x half-width) OUTER-CIRCLE-RADIUS) y scene)))
    
    ;; test methods, to probe the state.
    ;; -> Nat
    (define/public (for-test:x) x)
    ;; -> Nat
    (define/public (for-test:y) y)
    ;; -> Nat
    (define/public (for-test:particle-x) particle-x)
    ;; -> Nat
    (define/public (for-test:particle-y) particle-y)   
    ;; -> Boolean
    (define/public (for-test:selected?) selected?)
    
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define xc (new XController% [model (new Model%)])))
    
    (send xc after-drag (+ CANVAS-CENTER-X 5) (+ CANVAS-CENTER-Y 5))
    (check-false
     (send xc for-test:selected?)
     "Not Selected Controller after drag.")
    
    (check-equal?
     (send xc for-test:x)
     CANVAS-CENTER-X
     "Not Selected Controller after drag.")
    
    (check-equal?
     (send xc for-test:y)
     CANVAS-CENTER-Y
     "Not Selected Controller after drag.")
    
    (check-equal?
     (send xc for-test:particle-x)
     75
     "Not Selected Controller after drag.")
    
    (check-equal?
     (send xc for-test:particle-y)
     50
     "Not Selected Controller after drag.")
    
    (send xc after-button-down CANVAS-CENTER-X CANVAS-CENTER-Y)
    (check-true
     (send xc for-test:selected?)
     "Controller after button down.")
    
    (check-equal?
     (send xc for-test:x)
     CANVAS-CENTER-X
     "Controller after button down.")
    
    (check-equal?
     (send xc for-test:y)
     CANVAS-CENTER-Y
     "Controller after button down.")
    
    (check-equal?
     (send xc for-test:particle-x)
     75
     "Controller after button down.")
    
    (check-equal?
     (send xc for-test:particle-y)
     50
     "Controller after button down.")
    
    (send xc after-drag (+ CANVAS-CENTER-X 5) (+ CANVAS-CENTER-Y 5))
    (check-true
     (send xc for-test:selected?)
     "Controller after drag")
    
    ;; Methods defined to call the super class methods only
    (send xc within-limits 10 15 20)
    (send xc in-this? 50 50 100 100 200 200)
    (send xc current-color true)
    (send xc add-to-scene (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
    
    (send xc after-button-up (+ CANVAS-CENTER-X 5) (+ CANVAS-CENTER-Y 5))
    (check-false
     (send xc for-test:selected?)
     "Controller after button up.")
    
    (check-equal?
     (send xc for-test:x)
     CANVAS-CENTER-X
     "Controller after button up.")
    
    (check-equal?
     (send xc for-test:y)
     CANVAS-CENTER-Y
     "Controller after button up.")
    
    (check-equal?
     (send xc for-test:particle-x)
     80
     "Controller after button up.")
    
    (check-equal?
     (send xc for-test:particle-y)
     50
     "Controller after button up.")
    ))