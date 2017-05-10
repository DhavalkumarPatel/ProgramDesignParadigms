;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; fsm.rkt : finite state machine for accepting strings that exactly
;; match the regular expression (a | b)* c (a | b)* d (e | f)*

(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)

(provide
 initial-state
 next-state
 accepting-state?
 error-state?)

;; check the location of file for automated testing
;; (check-location "02" "fsm.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

(define INITIAL 0)
(define INTERMEDIATE 1)
(define ACCEPTING 2)
(define ERROR -1)

;; a State is one of
;; -- INITIAL
;; -- INTERMEDIATE 
;; -- ACCEPTING
;; -- ERROR
;; INTERPRETATION:
;; INITIAL is the initial state, "a" and "b" machine input moves us to same
;; state, "c" moves us to INTERMEDIATE state and other inputs leads to ERROR state
;; INTERMEDIATE is the intermediate state, "a" and "b" machine input moves us to
;; same state, "d" moves us to ACCEPTING state and other inputs leads to ERROR state
;; ACCEPTING is the final state, "e" and "f" machine input moves us to same state
;; and other inputs leads to ERROR state
;; ERROR is the error state and from this there is no path (empty or non-empty) to
;; an ACCEPTING state

;; state-fn : State -> ??
#|
(define (state-fn state)
 (cond
   [(= state INITIAL) ...]
   [(= state INTERMEDIATE) ...]
   [(= state ACCEPTING) ...]
   [(= state ERROR) ...])) 
|#


;; a MachineInput is one of the string from
;; -- "a"
;; -- "b"
;; -- "c"
;; -- "d"
;; -- "e"
;; -- "f"
;; INTERPRETATION: self-evident

;; mi-fn : MachineInput -> ??
#|
(define (mi-fn mi)
 (cond
   [(string=? mi "a") ...]
   [(string=? mi "b") ...]
   [(string=? mi "c") ...]
   [(string=? mi "d") ...]
   [(string=? mi "e") ...]
   [(string=? mi "f") ...]))  
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-state : Number -> State
;; GIVEN: a number
;; RETURNS: a representation of the initial state
;; of your machine. The given number is ignored.

;; EXAMPLES\TESTS:

(begin-for-test

  (check-equal?
    (initial-state 0)
    INITIAL)

  (check-equal?
    (initial-state 5)
    INITIAL))

;; STRATEGY: Combine Simpler functions

(define (initial-state n)
  INITIAL)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; next-state : State MachineInput -> State
;; GIVEN: a state of the machine s and a machine input mi
;; RETURNS: the state that should follow the given input.

;; if s = INITIAL then for "a" and "b" mi it returns INITIAL, for "c"
;; it returns INTERMEDIATE, and for other mi it returns ERROR state

;; if s = INTERMEDIATE then for "a" and "b" mi it returns INTERMEDIATE, for "d"
;; it returns ACCEPTING, and for other mi it returns ERROR state

;; if s = ACCEPTING then for "e" and "f" mi it returns ACCEPTING, and for
;; other mi it returns ERROR state

;; if s = ERROR then for all possible mi it returns ERROR state.


;; EXAMPLES\TESTS:

(begin-for-test
  
  (check-equal? (next-state INITIAL "a") INITIAL)
  (check-equal? (next-state INITIAL "c")  INTERMEDIATE)
  (check-equal? (next-state INITIAL "d")  ERROR)

  (check-equal? (next-state INTERMEDIATE "a")  INTERMEDIATE)
  (check-equal? (next-state INTERMEDIATE "c")  ERROR)
  (check-equal? (next-state INTERMEDIATE "d")  ACCEPTING)
  
  (check-equal? (next-state ACCEPTING "a")  ERROR)
  (check-equal? (next-state ACCEPTING "e")  ACCEPTING)
  
  (check-equal? (next-state ERROR "a")  ERROR))

;; STRATEGY: Use template for State on s

(define (next-state s mi)
  (cond
    [(= s INITIAL) (state-after-initial mi)]
    [(= s INTERMEDIATE) (state-after-intermediate mi)]
    [(= s ACCEPTING) (state-after-accepting mi)]
    [(= s ERROR) ERROR]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; state-after-initial : MachineInput -> State
;; GIVEN: a machine input mi
;; RETURNS: the state after following mi from INITIAL state.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (state-after-initial "a") INITIAL)
  (check-equal? (state-after-initial "c") INTERMEDIATE)
  (check-equal? (state-after-initial "e") ERROR))

;; STRATEGY: Cases on MachineInput
  
(define (state-after-initial mi)
  (cond
    [(or (string=? mi "a") (string=? mi "b")) INITIAL]
    [(string=? mi "c") INTERMEDIATE]
    [else ERROR]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; state-after-intermediate : MachineInput -> State
;; GIVEN: a machine input mi
;; RETURNS: the state after following mi from INTERMEDIATE state.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (state-after-intermediate "a") INTERMEDIATE)
  (check-equal? (state-after-intermediate "c") ERROR)
  (check-equal? (state-after-intermediate "d") ACCEPTING))

;; STRATEGY: Cases on MachineInput
  
(define (state-after-intermediate mi)
  (cond
    [(or (string=? mi "a") (string=? mi "b")) INTERMEDIATE]
    [(string=? mi "d") ACCEPTING]
    [else ERROR]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; state-after-accepting : MachineInput -> State
;; GIVEN: a machine input mi
;; RETURNS: the state after following mi from ACCEPTING state.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (state-after-accepting "e") ACCEPTING)
  (check-equal? (state-after-accepting "a") ERROR))

;; STRATEGY: Cases on MachineInput

(define (state-after-accepting mi)
  (cond
    [(or (string=? mi "e") (string=? mi "f")) ACCEPTING]
    [else ERROR]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; accepting-state? : State -> Boolean
;; GIVEN: a state of the machine
;; RETURNS: true if the given state is a final (accepting) state

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (accepting-state? INITIAL) false)
  (check-equal? (accepting-state? ACCEPTING) true))

;; STRATEGY: Cases on State

(define (accepting-state? state)
  (= state ACCEPTING))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;error-state? : State -> Boolean
;;GIVEN: a state of the machine
;;RETURNS: true if there is no path (empty or non-empty) from the given
;;state to an accepting state

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? (error-state? ACCEPTING) false)
  (check-equal? (error-state? ERROR) true))

;; STRATEGY: Cases on State

(define (error-state? state)
  (= state ERROR))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

;; given regular expression will accept cd but will not accept abc

(begin-for-test
  (check-equal?
   (accepting-state?
    (next-state
     (next-state
      (initial-state 0)
      "c")
     "d"))
   true))

(begin-for-test
  (check-equal?
   (accepting-state?
    (next-state
     (next-state
      (next-state
       (initial-state 0)
       "a")
      "b")
     "c"))
   false))
