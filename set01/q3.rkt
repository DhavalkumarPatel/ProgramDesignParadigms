;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; q3.rkt: counts the number of pixels in a given image

(require rackunit)
(require "extras.rkt")

(provide image-area) 

;; DATA DEFINITIONS: none

;; image-area: Image -> NonNegReal          
;; GIVEN: an Image img
;; RETURNS: the number of pixels in img

;; EXAMPLES/TESTS:
(begin-for-test
  (check-equal? (image-area (rectangle 5 7 "solid" "blue")) 35 
    "The number of pixels of given rectangle should be 35")
  (check-equal? (image-area (circle 10 "solid" "blue")) 400 
    "The number of pixels of given circle should be 400"))

;; DESIGN STRATEGY: Combine simpler functions

(define (image-area img)
  (* (image-height img) (image-width img)))