;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname coffee-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; coffee-machine.rkt : Generates the coffee machine

(require rackunit)
(require "extras.rkt")

(provide
 initial-machine
 machine-next-state
 machine-output
 machine-remaining-coffee
 machine-remaining-chocolate
 machine-bank)

;; check the location of file for automated testing
;; (check-location "02" "coffee-machine.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

;; cost of one coffee in cents
(define COFFEE-COST 150)

;; cost of one hot chocolate in cents
(define HOT-CHOCOLATE-COST 60)

(define COFFEE "coffee")
(define HOT-CHOCOLATE "hot chocolate")
(define CHANGE "change")
(define OUT-OF-ITEM "Out of Item")
(define NOTHING "Nothing")

;; A CustomerInput is one of
;; -- a PosInt        interp: insert the specified amount of money, in cents
;; -- COFFEE          interp: request a coffee
;; -- HOT-CHOCOLATE   interp: request a hot chocolate
;; -- CHANGE          interp: return all the unspent money that the customer has inserted

;; customer-input-fn : CustomerInput -> ??
#|
(define (customer-input-fn ci)
  (cond
    [(and (integer? ci) (positive? ci)) ...]
    [(string=? ci COFFEE) ...]
    [(string=? ci HOT-CHOCOLATE) ...]
    [(string=? ci CHANGE) ...]))
|#


;; A MachineOutput is one of
;; -- COFFEE         interp: machine dispenses a cup of coffee
;; -- HOT-CHOCOLATE  interp: machine dispenses a cup of hot chocolate
;; -- OUT-OF-ITEM    interp: machine displays OUT-OF-ITEM
;; -- a PosInt       interp: machine releases the specified amount of money, in cents
;; -- NOTHING        interp: the machine does nothing

;; machine-output-fn : MachineOutput -> ??
#|
(define (machine-output-fn mo)
  (cond
    [(string=? mo COFFEE) ...]
    [(string=? mo HOT-CHOCOLATE) ...]
    [(string=? mo OUT-OF-ITEM) ...]
    [(and (integer? mo) (positive? mo)) ...]
    [(string=? mo NOTHING) ...]))
|#


(define-struct machine-state (coffee-cups hot-chocolates bank unspent-amount))

;; A MachineState is a 
;; (make-machine-state NonNegInt NonNegInt NonNegInt NonNegInt)
;; INTERPRETATION:
;; coffee-cups is the number of available coffee cups in machine
;; hot-chocolates is the number of available hot chocolates in machine
;; bank is the container which contains all the money machine has kept from customer's purchases (in cents)
;; unspent-amount is the unspent money that the customer has put in during transaction (in cents)

;; machine-state-fn : MachineState -> ??
#|
(define (machine-state-fn ms)
  (...
    (machine-state-coffee-cups ms)
    (machine-state-hot-chocolates ms)
    (machine-state-bank ms)
    (machine-state-unspent-amount ms)))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of cups of coffee and of hot chocolate
;; RETURNS: the state of a machine loaded with the given number of cups
;; of coffee and of hot chocolate, with an empty bank.

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (initial-machine 5 4) 
   (make-machine-state 5 4 0 0)))

;; DESIGN STRATEGY: combine simpler functions

(define (initial-machine cc hc)
  (make-machine-state cc hc 0 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;; input

;; EXAMPLES\TESTS:

(begin-for-test

  ;; Currency inserted
  (check-equal? 
   (machine-next-state (make-machine-state 5 5 0 0) 250) 
   (make-machine-state 5 5 0 250))

  ;; COFFEE ordered
  (check-equal? 
   (machine-next-state (make-machine-state 5 5 0 250) COFFEE) 
   (make-machine-state 4 5 150 100))

  ;; Hot
  (check-equal? 
   (machine-next-state (make-machine-state 4 5 150 100) HOT-CHOCOLATE) 
   (make-machine-state 4 4 210 40))

  (check-equal? 
   (machine-next-state (make-machine-state 4 4 210 40) CHANGE) 
   (make-machine-state 4 4 210 0)))

;; DESIGN STRATEGY: Use template for CustomerInput on ci

(define (machine-next-state ms ci)
  (cond
    [(and (integer? ci) (positive? ci)) (machine-state-after-money-inserted ms ci)]
    [(string=? ci COFFEE) (machine-state-after-coffee-requested ms)]
    [(string=? ci HOT-CHOCOLATE) (machine-state-after-hot-chocolate-requested ms)]
    [(string=? ci CHANGE) (machine-state-after-change-requested ms)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-state-after-money-inserted : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine after customer inserts money
;; WHERE: CustomerInput must be a PosInt

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-state-after-money-inserted (make-machine-state 5 5 0 0) 250) 
   (make-machine-state 5 5 0 250)))

;; DESIGN STRATEGY: Use template for MachineState on ms

(define (machine-state-after-money-inserted ms ci)
  (make-machine-state
   (machine-state-coffee-cups ms)
   (machine-state-hot-chocolates ms)
   (machine-state-bank ms)
   (+ (machine-state-unspent-amount ms) ci)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-state-after-coffee-requested : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: the state of the machine that should follow the customer's
;; coffee request

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-state-after-coffee-requested (make-machine-state 5 6 150 160)) 
   (make-machine-state 4 6 300 10))

  (check-equal? 
   (machine-state-after-coffee-requested (make-machine-state 0 4 150 160)) 
   (make-machine-state 0 4 150 160))

  (check-equal? 
   (machine-state-after-coffee-requested (make-machine-state 5 4 150 100)) 
   (make-machine-state 5 4 150 100))

  (check-equal? 
   (machine-state-after-coffee-requested (make-machine-state 0 4 150 100)) 
   (make-machine-state 0 4 150 100)))

;; DESIGN STRATEGY: Cases on MachineState

(define (machine-state-after-coffee-requested ms)
  (if

   (and
    (> (machine-state-coffee-cups ms) 0)
    (>= (machine-state-unspent-amount ms) COFFEE-COST))

   (make-machine-state
    (- (machine-state-coffee-cups ms) 1)
    (machine-state-hot-chocolates ms)
    (+ (machine-state-bank ms) COFFEE-COST)
    (- (machine-state-unspent-amount ms) COFFEE-COST))

   ms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-state-after-hot-chocolate-requested : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: the state of the machine that should follow the customer's
;; hot chocolate request

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-state-after-hot-chocolate-requested (make-machine-state 5 4 150 70)) 
   (make-machine-state 5 3 210 10))

  (check-equal? 
   (machine-state-after-hot-chocolate-requested (make-machine-state 5 0 150 70)) 
   (make-machine-state 5 0 150 70))

  (check-equal? 
   (machine-state-after-hot-chocolate-requested (make-machine-state 5 4 150 50)) 
   (make-machine-state 5 4 150 50))

  (check-equal? 
   (machine-state-after-hot-chocolate-requested (make-machine-state 5 0 150 50)) 
   (make-machine-state 5 0 150 50)))

;; DESIGN STRATEGY: Cases on MachineState

(define (machine-state-after-hot-chocolate-requested ms)
  (if
   (and
    (> (machine-state-hot-chocolates ms) 0)
    (>= (machine-state-unspent-amount ms) HOT-CHOCOLATE-COST))
   
   (make-machine-state
    (machine-state-coffee-cups ms)
    (- (machine-state-hot-chocolates ms) 1)
    (+ (machine-state-bank ms) HOT-CHOCOLATE-COST)
    (- (machine-state-unspent-amount ms) HOT-CHOCOLATE-COST))
   
   ms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-state-after-change-requested : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: the state of the machine that should follow the customer's
;; change request

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-state-after-change-requested (make-machine-state 5 4 150 200)) 
   (make-machine-state 5 4 150 0)))

;; DESIGN STRATEGY: Use template for MachineState on ms

(define (machine-state-after-change-requested ms)
  (make-machine-state
   (machine-state-coffee-cups ms)
   (machine-state-hot-chocolates ms)
   (machine-state-bank ms)
   0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;; customer input


;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-output (make-machine-state 5 4 0 0) 250) 
   NOTHING)

  (check-equal? 
   (machine-output (make-machine-state 5 4 0 250) COFFEE) 
   COFFEE)

  (check-equal? 
   (machine-output (make-machine-state 5 4 0 100) HOT-CHOCOLATE) 
   HOT-CHOCOLATE)

  (check-equal? 
   (machine-output (make-machine-state 5 4 0 40) CHANGE) 
   40))

;; DESIGN STRATEGY: Use template for CustomerInput on ci

(define (machine-output ms ci)
  (cond
    [(and (integer? ci) (positive? ci)) NOTHING]
    [(string=? ci COFFEE) (machine-output-after-coffee-requested ms)]
    [(string=? ci HOT-CHOCOLATE) (machine-output-after-hot-chocolate-requested ms)]
    [(string=? ci CHANGE) (machine-output-after-change-requested ms)]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-output-after-coffee-requested : MachineState -> MachineOutput
;; GIVEN: a machine state
;; RETURNS: the output of the machine on customer's request for coffee

;; EXAMPLES\TESTS:

(begin-for-test
  
  (check-equal? 
   (machine-output-after-coffee-requested (make-machine-state 5 4 0 150)) 
   COFFEE)
  
  (check-equal? 
   (machine-output-after-coffee-requested (make-machine-state 5 4 0 160)) 
   COFFEE)
  
  (check-equal? 
   (machine-output-after-coffee-requested (make-machine-state 0 4 0 160)) 
   OUT-OF-ITEM)
  
  (check-equal? 
   (machine-output-after-coffee-requested (make-machine-state 0 4 0 100)) 
   OUT-OF-ITEM)
  
  (check-equal? 
   (machine-output-after-coffee-requested (make-machine-state 5 4 0 100)) 
   NOTHING))

;; DESIGN STRATEGY: Cases on MachineState

(define (machine-output-after-coffee-requested ms)
  (if

   (> (machine-state-coffee-cups ms) 0)

   (if
    (>= (machine-state-unspent-amount ms) COFFEE-COST)
    COFFEE
    NOTHING)

   OUT-OF-ITEM))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-output-after-hot-chocolate-requested : MachineState -> MachineOutput
;; GIVEN: a machine state
;; RETURNS: the output of the machine on customer's request for hot chocolate

;; EXAMPLES\TESTS:

(begin-for-test
  
  (check-equal? 
   (machine-output-after-hot-chocolate-requested (make-machine-state 5 4 0 60)) 
   HOT-CHOCOLATE)
  
  (check-equal? 
   (machine-output-after-hot-chocolate-requested (make-machine-state 5 4 0 70)) 
   HOT-CHOCOLATE)
  
  (check-equal? 
   (machine-output-after-hot-chocolate-requested (make-machine-state 5 0 0 70)) 
   OUT-OF-ITEM)
  
  (check-equal? 
   (machine-output-after-hot-chocolate-requested (make-machine-state 5 0 0 50)) 
   OUT-OF-ITEM)
  
  (check-equal? 
   (machine-output-after-hot-chocolate-requested (make-machine-state 5 4 0 50)) 
   NOTHING))

;; DESIGN STRATEGY: Cases on MachineState

(define (machine-output-after-hot-chocolate-requested ms)
  (if
   
   (> (machine-state-hot-chocolates ms) 0)
   
   (if
    (>= (machine-state-unspent-amount ms) HOT-CHOCOLATE-COST)
    HOT-CHOCOLATE
    NOTHING)

   OUT-OF-ITEM))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-output-after-change-requested : MachineState -> MachineOutput
;; GIVEN: a machine state
;; RETURNS: the output of the machine on customer's request for change

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-output-after-change-requested (make-machine-state 5 4 210 0)) 
   NOTHING)

  (check-equal? 
   (machine-output-after-change-requested (make-machine-state 5 4 210 40)) 
   40))

;; DESIGN STRATEGY: Cases on MachineState

(define (machine-output-after-change-requested ms)
  (if
   (> (machine-state-unspent-amount ms) 0)
   (machine-state-unspent-amount ms)
   NOTHING))
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-remaining-coffee : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of cups of coffee left in the machine

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-remaining-coffee (make-machine-state 5 4 210 0)) 
   5))

;; DESIGN STRATEGY: Use template for MachineState on ms
  
(define (machine-remaining-coffee ms)
  (machine-state-coffee-cups ms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-remaining-chocolate : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of hot chocolates left in the machine

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-remaining-chocolate (make-machine-state 5 4 210 0)) 
   4))

;; DESIGN STRATEGY: Use template for MachineState on ms

(define (machine-remaining-chocolate ms)
  (machine-state-hot-chocolates ms))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents

;; EXAMPLES\TESTS:

(begin-for-test
  (check-equal? 
   (machine-bank (make-machine-state 5 4 210 0)) 
   210))

;; DESIGN STRATEGY: Use template for MachineState on ms

(define (machine-bank ms)
  (machine-state-bank ms))