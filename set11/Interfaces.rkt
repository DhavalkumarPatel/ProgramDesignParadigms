#lang racket

(provide SWidget<%>
         Controller<%>
         Model<%>
         SubControllerHooks<%>
         PosVelControllerHooks<%>
         MouseController<%>
         MouseControllerHooks<%>
         X-PARTICLE-LIMIT
         Y-PARTICLE-LIMIT
         HANDLER-HALF-WIDTH
         HANDLER-HALF-HEIGHT
         CANVAS-WIDTH
         CANVAS-HEIGHT
         CANVAS-CENTER-X
         CANVAS-CENTER-Y
         DISPLAY-BUFFER
         VOID)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS:

(define X-PARTICLE-LIMIT 150)
(define Y-PARTICLE-LIMIT 100)
(define HANDLER-HALF-WIDTH 5)
(define HANDLER-HALF-HEIGHT 5)
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)
(define CANVAS-CENTER-X (/ CANVAS-WIDTH 2))
(define CANVAS-CENTER-Y (/ CANVAS-HEIGHT 2))
(define DISPLAY-BUFFER 30)
(define VOID 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; INTERFACES:

;; Every stable (stateful) object that lives in the world must implement the
;; SWidget<%> interface.
(define SWidget<%>
  (interface ()
    
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this widget to the state it should have
    ;; following a tick.
    after-tick          
    
    ;; Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event
    after-key-event     
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    add-to-scene
    ))

;; Every model object that lives in the world must implement the
;; Model<%> interface.
(define Model<%>
  (interface ()
    
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this model to the state it should have
    ; following a tick.
    after-tick        
    
    ;; Controller<%> -> Void
    ;; Registers the given controller to receive signal
    register          
    
    ;; Command -> Void
    ;; Executes the given command
    execute-command   
    ))

;; Every controller object that lives in the world must implement the
;; Controller<%> interface.
(define Controller<%>    
  (interface (SWidget<%>)
    
    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    receive-signal

    ;; Boolean -> Color
    ;; RETURNS: returns red color if the given boolean is true else black
    current-color

    ;; Nat Nat Nat Nat Nat Nat -> Void
    ;; GIVEN: x and y coordinate of any point in canvas, x and y coordinate of center of
    ;;        the rectangle and width and height of that rectangle
    ;; RETURNS: true iff the given point is in side the given rectangle
    in-this?
    ))

;; Every controller object that lives in the world must implement the
;; MouseController<%> interface.
(define MouseController<%>    
  (interface ()

    ;; Number^3 -> Number
    ;; GIVEN: three numbers lo, val and hi
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    within-limits
    
    ))

;; Hook Interface : Open hooks (abstract methods): this methods must be supplied by each
;; subclass of Controller%.
(define SubControllerHooks<%>    
  (interface ()

    ;; Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this particle to the state it should have
    ;; following the button down event at the given location.
    select-controller

    ;; Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this particle to the state it should have
    ;; following the drag event at the given location.
    drag-particle
    ))

;; Hook Interface : Open hooks (abstract methods): this methods must be supplied by each
;; subclass of MouseController%.
(define MouseControllerHooks<%>    
  (interface ()

    ;; Nat Nat -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this particle to the state it should have
    ;; following the drag event at the given location.
    drag-particle
    ))

;; Hook Interface : Open hooks (abstract methods): this methods must be supplied by each
;; subclass of PosVelController%.
(define PosVelControllerHooks<%>    
  (interface ()
    
    ;; Nat Nat -> Command
    ;; RETURNS: the specific command for model to execute based on given x,y coordinates
    ;; of particle.
    key-event-action
    
    ;; -> String
    ;; RETURNS: the name of controller to display
    get-name
    
    ))

;; protocol: 
;; model sends the controller an initialization signal as soon as it registers.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; provide the structs for Command and Signal
(provide 
 (struct-out set-position) 
 (struct-out incr-velocity)
 (struct-out report-position)
 (struct-out report-velocity))

(define-struct set-position (pos-x pos-y) #:transparent)
(define-struct incr-velocity (vx vy) #:transparent)
;; A Command is one of
;; -- Boolean
;;    INTERP: selects or unselects the particle
;; -- (make-set-position Real Real)
;;    INTERP: set the position(x, y co-ordinates) of the particle to pos-x, pos-y
;; -- (make-incr-velocity Real Real)
;;    INTERP: increment the x and y velocity of the particle to vx and vy (pixel/sec)


(define-struct report-position (pos-x pos-y) #:transparent)
(define-struct report-velocity (vx vy) #:transparent)
;; A Signal is one of
;; -- (make-report-position Real Real)
;;    INTERP: report the current position(x, y co-ordinates) of the particle
;; -- (make-report-velocity Real Real)
;;    INTERP: report the x and y velocity of the particle of the particle