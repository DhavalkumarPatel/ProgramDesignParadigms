;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q1.rkt: Computes the distance of given point to the origin

(require rackunit)
(require "extras.rkt")

(provide distance-to-origin) 

;; DATA DEFINITIONS: none

;; distance-to-origin: Real Real -> NonNegReal          
;; GIVEN: the x and y co-ordinates of a point 
;; RETURNS: the distance of point(x,y) to the origin using formula (square root of (x^2 + y^2))

;; EXAMPLES:
;; (distance-to-origin 0 0) = 0
;; (distance-to-origin 3 4) = 5
;; (distance-to-origin -3 -4) = 5

;; DESIGN STRATEGY: Combine simpler functions

(define (distance-to-origin x y)
  (sqrt (+ (expt x 2) (expt y 2))))

;; TESTS:
(begin-for-test
  (check-equal? (distance-to-origin 0 0) 0 
    "The distance of point(0,0) to the origin should be 0")
  (check-equal? (distance-to-origin 3 4) 5 
    "The distance of point(3,4) to the origin should be 5")
  (check-equal? (distance-to-origin -3 -4) 5 
    "The distance of point(-3,-4) to the origin should be 5"))
