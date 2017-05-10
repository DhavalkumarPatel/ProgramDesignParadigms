#lang racket
;; This is a dummy model class used for testing only.

(require "extras.rkt")
(require "Interfaces.rkt")

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
    ))