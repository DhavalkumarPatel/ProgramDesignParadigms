#lang racket
;; the model consists of a particle, bouncing with its center from x=0 to x=150 and
;; y=0 to y=100. It accepts commands and reports when its status changes.

(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "VelocityController.rkt")
(require "PositionController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "XYController.rkt")


(provide Model%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

;; A ListOfController is one of
;; -- empty
;; -- (cons Controller ListOfController)
;; TEMPLATE:
;; loc-fn : ListOfController -> ??
#|
(define (loc-fn loc)
  (cond
    [(empty? loc) ...]
    [else (... (first loc)
               (loc-fn (rest loc)))]))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CLASS DEFINITION:


;; A Model is a
;; (new Model% [x Nat] [y Nat] [vx Nat] [vy Nat] [mouse-controller-selected? Boolean]
;;             [controllers ListOfController])
;; A Model represents a model in the world which interacts with the registerd controllers.
(define Model%
  (class* object% (Model<%>)
    
    ;; x and y boundaries for the particle
    (field [lo-x 0])
    (field [hi-x X-PARTICLE-LIMIT])
    (field [lo-y 0])
    (field [hi-y Y-PARTICLE-LIMIT])
    
    ;; the x and y position of the center of particle
    (init-field [x (/ (+ lo-x hi-x) 2)])
    (init-field [y (/ (+ lo-y hi-y) 2)])
    
    ;; the velocities of particle in x and y direction
    (init-field [vx 0])
    (init-field [vy 0])
    
    ;; the controller with particle is selected or not, default is false.
    (init-field [mouse-controller-selected? false])
    
    ;; list of registered controllers with this model
    (init-field [controllers empty])   
    
    (super-new)
    
    ;; -> Void
    ;; EFFECT: if the controller is selected then after-tick have following effects:
    ;; 1. Moves the object as per vx and vy velocities
    ;; 2. Reports the position to all registered controllers
    ;; 3. Reverse the vx and vy velocities if object is going beyond the boundaries
    ;; 4. Reports the velocity to all registered controllers
    ;; STRATEGY: Cases on weather controller object is selected or not
    (define/public (after-tick)
      (if (not mouse-controller-selected?)
          (begin
            (set! x (within-limits lo-x (+ x vx) hi-x))
            (set! y (within-limits lo-y (+ y vy) hi-y))
            (publish-position)
            (if (or (= x hi-x) (= x lo-x))
                (set! vx (- vx))
                VOID)
            (if (or (= y hi-y) (= y lo-y))
                (set! vy (- vy))
                VOID)
            (publish-velocity))
          VOID))
    
    ;; Number^3 -> Number
    ;; GIVEN: three numbers lo, val and hi
    ;; WHERE: lo <= hi
    ;; RETURNS: val, but limited to the range [lo,hi]
    (define (within-limits lo val hi)
      (max lo (min val hi)))
    
    ;; Controller -> Void
    ;; register the new controller and reports it the x,y position and vx, vy velocities
    ;; of the object
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x y))
        (send c receive-signal (make-report-velocity vx vy))))
    
    ;; Command -> Void
    ;; decodes the command, executes it, and sends updates to the
    ;; controllers.
    ;; STRATEGY: Cases on Command cmd
    (define/public (execute-command cmd)
      (cond
        [(boolean? cmd)(set! mouse-controller-selected? cmd)]
        [(set-position? cmd)
         (begin
           (set! x (set-position-pos-x cmd))
           (set! y (set-position-pos-y cmd))
           (publish-position))]
        [(incr-velocity? cmd)
         (begin
           (set! vx (+ vx (incr-velocity-vx cmd)))
           (set! vy (+ vy (incr-velocity-vy cmd)))
           (publish-velocity))]))
    
    ;; -> Void
    ;; report the position of particle to each registered controller.
    ;; STRATEGY: Use HOF for-each on controllers
    (define (publish-position)
      (let ((msg (make-report-position x y)))
        (for-each
         (lambda (obs) (send obs receive-signal msg))
         controllers)))
    
    ;; -> Void
    ;; report the velocity of particle to each registered controller.
    ;; STRATEGY: Use HOF for-each on controllers
    (define (publish-velocity)
      (let ((msg (make-report-velocity vx vy)))
        (for-each
         (lambda (obs) (send obs receive-signal msg))
         controllers)))
    
    ;; test methods, to probe the state.
    ;; -> Nat
    (define/public (for-test:x) x)
    ;; -> Nat
    (define/public (for-test:y) y)
    ;; -> Nat
    (define/public (for-test:vx) vx)
    ;; -> Nat
    (define/public (for-test:vy) vy)   
    ;; -> Boolean
    (define/public (for-test:mouse-controller-selected?) mouse-controller-selected?)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; TESTS:

(begin-for-test
  (local
    ((define m (new Model%))
     (define x (new XController% [model m]))
     (define y (new YController% [model m]))
     (define xy (new XYController% [model m]))
     (define pos (new PositionController% [model m]))
     (define val (new VelocityController% [model m])))
    
    (send m after-tick)
    (check-false
     (send m for-test:mouse-controller-selected?)
     "for-test:mouse-controller-selected? After Tick")
    
    (check-equal?
     (send m for-test:x)
     75
     "for-test:x After Tick")
    
    (check-equal?
     (send m for-test:y)
     50
     "for-test:y After Tick")
    
    (check-equal?
     (send m for-test:vx)
     0
     "for-test:vx After Tick")
    
    (check-equal?
     (send m for-test:vy)
     0
     "for-test:vy After Tick")
    
    (send m execute-command (make-set-position X-PARTICLE-LIMIT Y-PARTICLE-LIMIT))
    (send m execute-command (make-incr-velocity 10 10))    
    (send m after-tick)
     (check-false
     (send m for-test:mouse-controller-selected?)
     "for-test:mouse-controller-selected? After Tick")
    
    (check-equal?
     (send m for-test:x)
     150
     "for-test:x After Tick")
    
    (check-equal?
     (send m for-test:y)
     100
     "for-test:y After Tick")
    
    (check-equal?
     (send m for-test:vx)
     -10
     "for-test:vx After Tick")
    
    (check-equal?
     (send m for-test:vy)
     -10
     "for-test:vy After Tick")
    
    (send m execute-command true)
    (send m after-tick)
     (check-true
     (send m for-test:mouse-controller-selected?)
     "for-test:mouse-controller-selected? After Tick")
    
    (check-equal?
     (send m for-test:x)
     150
     "for-test:x After Tick")
    
    (check-equal?
     (send m for-test:y)
     100
     "for-test:y After Tick")
    
    (check-equal?
     (send m for-test:vx)
     -10
     "for-test:vx After Tick")
    
    (check-equal?
     (send m for-test:vy)
     -10
     "for-test:vy After Tick")
    ))