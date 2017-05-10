;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rainfall) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; rainfall.rkt: Produce the average of daily rainfall amounts.

;; It consumes a list of numbers representing daily rainfall amounts. The list may
;; contain the number -999 indicating the end of the data of interest. Produce the
;; average of the non-negative values in the list up to the first -999 (if it shows up).

(require rackunit)
(require "extras.rkt")

(provide
 rainfall)

;; check the location of file for automated testing
;; (check-location "07" "rainfall.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; CONSTANTS

(define NUMBER-AT-LIST-ENDS -999)
(define DEFAULT-AVERAGE 0)
(define INITIAL-SUM 0)
(define INITIAL-COUNT 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; DATA DEFINITIONS:

;; A ListOfReal is one of
;; -- empty
;; -- (cons Real ListOfReal)

;; TEMPLATE:
;; lor-fn : ListOfReal -> ??
#|
(define (lor-fn lor)
  (cond
    [(empty? lor) ...]
    [else (... (first lor)
               (lor-fn (rest lor)))]))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; rainfall : ListOfReal -> NonNegReal
;; GIVEN: the daily rainfall amounts lor.
;; RETURNS: the average of the non-negative values in the given list up to the
;; first -999 (if it shows up).

;; EXAMPLES:
;; (rainfall empty) = DEFAULT-AVERAGE
;; (rainfall (list 1 2 3 4 5 NUMBER-AT-LIST-ENDS 6)) = 3
;; (rainfall (list NUMBER-AT-LIST-ENDS 1 2 3 4 5 6)) = DEFAULT-AVERAGE
;; (rainfall (list -1 2 3 4 5 6 -7)) = 4

;; DESIGN STRATEGY: call a more general function
(define (rainfall lor)
  (average-rainfall lor INITIAL-SUM INITIAL-COUNT))


;; average-rainfall : ListOfReal NonNegReal NonNegInt -> NonNegReal
;; GIVEN: the sublist of daily rainfall amounts lor, sum and count
;; WHERE: sum and count represents the sum and count of all non-negative
;;        numbers occured in lor0 before the sublist lor.  
;; RETURNS: the average of the non-negative values in the list lor0 up to
;;          the first -999 (if it shows up).

;; EXAMPLES:
;; (average-rainfall empty INITIAL-SUM INITIAL-COUNT) = DEFAULT-AVERAGE
;; (average-rainfall (list 1 2 3 4 5 NUMBER-AT-LIST-ENDS 6) INITIAL-SUM INITIAL-COUNT) = 3

;; DESIGN STRATEGY: Use template for ListOfReal on lor
(define (average-rainfall lor sum count)  
  (cond
    [(empty? lor) (average sum count)]
    [else (cond
            [(= (first lor) NUMBER-AT-LIST-ENDS) (average sum count)]
            [(negative? (first lor)) (average-rainfall (rest lor) sum count)]
            [else (average-rainfall (rest lor) (+ sum (first lor)) (add1 count))])]))


;; average : NonNegReal NonNegInt -> NonNegReal
;; GIVEN: the sum and count of the list of real numbers
;; RETURNS: the average using formula (average=sum/count) if the count is greater
;;          than 0 else it returns the DEFAULT-AVERAGE

;; EXAMPLES:
;; (average INITIAL-SUM INITIAL-COUNT) = DEFAULT-AVERAGE
;; (average 15 5) = 3

;; DESIGN STRATEGY: Combine simpler functions
(define (average sum count)
  (if (> count INITIAL-COUNT)
      (/ sum count)
      DEFAULT-AVERAGE))

;; TESTS:
(begin-for-test
  (check-equal?
   (rainfall empty) DEFAULT-AVERAGE
   "The average of empty list should be zero.")
  
  (check-equal?
   (rainfall (list 1 2 3 4 5 NUMBER-AT-LIST-ENDS 6)) 3
   "The average of list should be 3.")
  
  (check-equal?
   (rainfall (list NUMBER-AT-LIST-ENDS 1 2 3 4 5 6)) DEFAULT-AVERAGE
   "The average of the list starts with NUMBER-AT-LIST-ENDS should be zero.")
  
  (check-equal?
   (rainfall (list -1 2 3 4 5 6 -7)) 4
   "The average of list should be 4."))

